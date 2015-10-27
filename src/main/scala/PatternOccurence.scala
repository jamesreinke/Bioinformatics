object PatternOccurrence {
	def apply(pattern: String, genome: String): Set[Int] = {
		var indicies = Set[Int]()
		val length = genome.length - pattern.length
		var i = 0
		while(i < length){
			val index = genome.indexOf(pattern, i)
			if(index > 0) {
				indicies += index
				i = index
			}
			i += 1
		} 
		indicies
	}
}