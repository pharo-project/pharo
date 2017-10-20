comparing
= other 
	^ (other class == self class) 
		and: [other alignment = alignment]