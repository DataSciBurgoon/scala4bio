package tests

import org.junit.Test
import org.junit.Assert._
import bio_objects.DNA
import bio_objects.RNA
import transcription.Transcriber

/**
 * @author burgoonl
 */
class TestTranscriber {
  @Test def test_transcriber(){
    val cdna_sequence = "ACCTGGCAT"
    val cdna = new DNA("test_cdna1", cdna_sequence)
    val transcriber = new Transcriber(cdna)
    val rna_seq = transcriber.transcribe()
    assertEquals("UGGACCGUA", rna_seq)
  }
}