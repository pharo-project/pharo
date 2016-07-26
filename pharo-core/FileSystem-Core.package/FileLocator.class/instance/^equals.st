comparing
= other
	^ self species = other species
		and: [origin = other origin
			and: [path = other path]]