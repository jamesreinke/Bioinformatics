object ChOne {

	/* 
		Given genome and kmer length, returns a set of all kmers that appear min times in a given window.
	*/
	def clumps(genome: String, k: Int, window: Int, min: Int): Set[String] = {
		/* Recursively adds sequences to a set who fullfill the parameters */
		def help(g: String, sequences: Set[String] = Set(), prev: Map[String, Int] = Map(), old: String): Set[String] = {
			g.length > window match {
				case true => {
					// shift the window over 1 nucleotide, generating new and old kmers
					val oldKmer: String = old + g.substring(0, k - 1)
					val newKmer: String = g.substring(0, k)
					val newMap: Map[String, Int] = prev + 
					  (oldKmer -> (prev.getOrElse(oldKmer, 1) - 1)) + 
					  (newKmer -> (prev.getOrElse(newKmer, 0) + 1))
					  if(newMap.getOrElse(newKmer, 0) > min) help(g.tail, sequences + newKmer, newMap, g.head.toString)
					  else help(g.tail, sequences, newMap, g.head.toString)
				}
				case false => sequences
			}
		}
		genome.length > window match {
			case true => {
			  // retrieve counts for the first sequence window
			  val counts = frequency(genome, k)
			  val sequences: Set[String] = counts.filter(x => x._2 > min).map(x => x._1).toSet
			  return help(genome.tail, sequences, counts, genome.head.toString) // frame shift
			}
			case false => return Set()
		}
		Set()
	}


	/* Finds the reverse complement of a sequence of nucleotides */
	def reverseComplement(s: String): String = {
		val rev = s.reverse
		rev map { x => x match {
			case 'A' => 'T'
			case 'T' => 'A'
			case 'C' => 'G'
			case 'G' => 'C'
			case _ => throw new Exception("Invalid nucleotide: " + x)
			}
		}
	}


	/* 
		Finds all indices where the given pattern exist in a sequence 
			Inefficient in that it will find the same indicies over and over again.
	*/
	def indicies(pattern: String, genome: String): Set[Int] = {
		var indicies = Set[Int]()
		val length = genome.length - pattern.length
		var i = 0
		while(i < length){
			val index = genome.indexOf(pattern, i)
			if(index > 0){
				indicies += index
				i = index
			}
			i += 1
		}
		indicies
	}


	/* 
		Takes the frequency counts for all kmers in a given genome. 
	*/
	def frequency(genome: String, k: Int, counts: Map[String, Int] = Map()): Map[String, Int] = {
		genome.length > k match {
			case true => {
				val kmer = genome.substring(0, k)
				frequency(genome.tail, k, counts + (kmer -> (counts.getOrElse(kmer, 0) + 1)))
			}
			case false => counts
		}
	}


	/* 
		Generates a list of all sequences of length k 
	*/
	def kmer(text: String, k: Int): List[String] = {
		val total = text.length - k
		val ret = for(i <- 0 to total) yield {
			text.substring(i, i + k)
		}
		ret.toList
	}

}