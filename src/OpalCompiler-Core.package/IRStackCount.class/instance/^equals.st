comparing
= other

	^ self class == other class 
	  and: [start = other start
	  and: [position = other position
	  and: [length = other size]]]