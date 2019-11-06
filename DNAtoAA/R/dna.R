#' DNA or RNA translation into amino acid code
#' @param x a text string containig a DNA or RNA sequence
#' @return A a text tring that returns a Translated seq
#' @examples
#' x <- "ATGTCCTTAGGCT"
#' Translate(x)
#' @export


Translate <- function(x){
  if(nchar(x)%%3 != 0){
    warning("Your submitted sequence length is not divisible by 3. Please submit a sequence that is divisible by 3.")
  }
  table <- matrix(c("TTT", "TTC", "TTA", "TTG", "TCT", "TCC", "TCA", "TCG", "TAT", "TAC", "TAA", "TAG", "TGT", "TGC", "TGA", "TGG",
                    "CTT", "CTC", "CTA", "CTG", "CCT", "CCC", "CCA", "CCG", "CAT", "CAC", "CAA", "CAG", "CGT", "CGC", "CGA", "CGG",
                    "ATT", "ATC", "ATA", "ATG", "ACT", "ACC", "ACA", "ACG", "AAT", "AAC", "AAA", "AAG", "AGT", "AGC", "AGA", "AGG",
                    "GTT", "GTC", "GTA", "GTG", "GCT", "GCC", "GCA", "GCG", "GAT", "GAC", "GAA", "GAG", "GGT", "GGC", "GGA", "GGG",
                    "F", "F", "L", "L", "S", "S", "S", "S", "Y", "Y", "*", "*", "C", "C", "*", "W",
                    "L", "L", "L", "L", "P", "P", "P", "P", "H", "H", "Q", "Q", "R", "R", "R", "R",
                    "I", "I", "I", "M", "T", "T", "T", "T", "N", "N", "K", "K", "S", "S", "R", "R",
                    "V", "V", "V", "V", "A", "A", "A", "A", "D", "D", "E", "E", "G", "G", "G", "G"), 64,2)

  dna <- toupper(x)
  dna <- gsub("U", "T", dna)
  # codons <- list()
  num.codons <- nchar(dna)/3
  pos.codon <- seq(from = 1, by = 3, length.out = num.codons)
  AA_seq <- c()
  for(i in pos.codon){
    codon <- substr(dna, i, i+2)
    if(codon %in% table[, 1]){
      hit <- which(codon == table[, 1])
      AA_seq <- c(AA_seq, table[hit, 2])
    }
  }
  return(AA_seq)
}

