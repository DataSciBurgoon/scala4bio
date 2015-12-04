package tests

import org.junit.Test
import org.junit.Assert._
import fasta.FastaParser
import bio_objects.RNA
import scala.collection.mutable.ArrayBuffer

/**
 * @author burgoonl
 */
class TestFastaParser {
  @Test def fasta_parser_test(){
    val file_path = "fasta_test.fsa"
    val fasta_parser = new FastaParser(file_path)
    val rna_seqs = fasta_parser.parse()
    assertEquals("test_gene_1", rna_seqs(0).annotation_string)
    assertEquals("CAUCAUG", rna_seqs(0).sequence)
    assertEquals("test_gene_2", rna_seqs(1).annotation_string)
    assertEquals("CAUCAUGCACACACA", rna_seqs(1).sequence)
    val translated_proteins = fasta_parser.parse_and_translate()
    assertEquals("test_gene_1_0", translated_proteins(0).annotation_string)
    assertEquals("HH", translated_proteins(0).sequence)
  }
  
  @Test def fasta_transcription(){
    val file_path = "cdna_test.fsa"
    val fasta_parser = new FastaParser(file_path)
    val rnas = fasta_parser.parse_and_transcribe_dna()
    assertEquals("test_gene_1_RNA", rnas(0).annotation_string)
    assertEquals("UUGGCCAAUUGGCCAA", rnas(0).sequence)
    assertEquals("UUUGGGCCCAAAUUUGGGCCCAAA", rnas(1).sequence)
    assertEquals("test_gene_2_RNA", rnas(1).annotation_string)
  }
  
}