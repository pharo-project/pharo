testing
includesCategory: aCategory
	^ (super includesCategory: aCategory) and: [ 
		self packages anySatisfy: [ :package | 
			package includesSystemCategory: aCategory ] ]