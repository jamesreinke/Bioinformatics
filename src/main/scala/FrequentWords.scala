object FrequentWords {

	def apply(text: String, k: Int): Set[String] = {
		val kmers = kmer(text, k)
		var max = 0
		var mostFrequent = Set[String]()
		var counts = Map[String, Int]()
		for(word <- kmers) {
			val count = counts.getOrElse(word, 0) + 1
			counts += (word -> count)
			if(count > max) max = count
		}
		for((word, count) <- counts) {
			if(count == max) mostFrequent += word
		}
		mostFrequent
	}


	/*
		Finds all of the k substrings within a text.
	*/
	private def kmer(text: String, k: Int): List[String] = {
		val total = text.length - k
		val ret = for(i <- 0 to total) yield {
			text.substring(i, i + k)
		}
		ret.toList
	}

}