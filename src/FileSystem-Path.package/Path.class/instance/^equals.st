comparing
= other
	^ self species = other species
		and: [self size = other size
			and: [(1 to: self size) allSatisfy: [:i | (self at: i) = (other at: i)]]]