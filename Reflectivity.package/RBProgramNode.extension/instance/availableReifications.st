*Reflectivity
availableReifications
	^RFReification subclasses 
		select: [ :e | e entities includesAny: self class withAllSuperclasses ]
		thenCollect: #key
