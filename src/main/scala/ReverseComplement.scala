/*
	Returns the reverse complement of a strand of nucleotides
*/
object ReverseComplement {
	def apply(s: String): String = {
		val rev = s.reverse
		rev map { x => x match {
			case 'A' => 'T'
			case 'T' => 'A'
			case 'C' => 'G'
			case 'G' => 'C'
			}
		}
	}
}