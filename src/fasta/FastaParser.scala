package fasta

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import bio_objects.DNA
import bio_objects.RNA
import bio_objects.Protein
import translation.Translator
import transcription.Transcriber

/**
 * @author burgoonl
 */
class FastaParser(val file_path:String) {
  def parse(): ArrayBuffer[RNA] = {
    var rna_seqs = ArrayBuffer.empty[RNA]
    var annotation = ""
    var sequence = ""
    var i = 1
    for(line <- Source.fromFile(file_path).getLines()){
      if(line.startsWith(">")){
        if(i > 1){
          rna_seqs += new RNA(annotation, sequence)
        }
        annotation = line.drop(1)
        sequence = ""
      }
      else{
        sequence += line.toUpperCase()
      }
      i += 1
    }
    rna_seqs += new RNA(annotation, sequence)
    return(rna_seqs)
    }
  
  def parse_and_transcribe_dna(): ArrayBuffer[RNA] = {
    var rnas = ArrayBuffer.empty[RNA]
    var annotation = ""
    var sequence = ""
    var i = 1
    for(line <- Source.fromFile(file_path).getLines()){
      if(line.startsWith(">")){
        if(i > 1){
          var dna = new DNA(annotation, sequence)
          rnas += transcribe(dna)          //this appends the output of transcribe into the ArrayBuffer protein_seqs
          
        }
        annotation = line.drop(1)
        sequence = ""
      }
      else{
        sequence += line.toUpperCase()
      }
      i += 1
    }
    var dna = new DNA(annotation, sequence)
    rnas += transcribe(dna)
    return(rnas)
  }
  
  def parse_and_translate(): ArrayBuffer[Protein] = {
    var protein_seqs = ArrayBuffer.empty[Protein]
    var annotation = ""
    var sequence = ""
    var i = 1
    for(line <- Source.fromFile(file_path).getLines()){
      if(line.startsWith(">")){
        if(i > 1){
          var rna = new RNA(annotation, sequence)
          protein_seqs ++= translate(rna)          //this appends the output of translate into the ArrayBuffer protein_seqs
        }
        annotation = line.drop(1)
        sequence = ""
      }
      else{
        sequence += line.toUpperCase()
      }
      i += 1
    }
    var rna = new RNA(annotation, sequence)
    protein_seqs ++= translate(rna)
    
    return(protein_seqs)
  }
  
  def transcribe(dna: DNA): RNA = {
    var transcriber = new Transcriber(dna)
    var rna_seq = transcriber.transcribe()
    val rna_annotation = dna.annotation_string + "_RNA"
    var rna = new RNA(rna_annotation, rna_seq)
    return(rna)
  }
  
  def translate(rna: RNA): ArrayBuffer[Protein] = {
    var protein_seqs = ArrayBuffer.empty[Protein]
    var protein_translator = new Translator(rna)
    var seqs = protein_translator.translate()
    for(i <- seqs.keys){
      var protein_annotation = rna.annotation_string + "_" + i
      var protein = new Protein(protein_annotation, seqs(i))
      protein_seqs += protein
    }
    return(protein_seqs)
  }
}