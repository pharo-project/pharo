building
remake: aClass
	self class update: aClass to: [ :anotherBuilder | 
		anotherBuilder fillFor: aClass.
		builder propagateChangesTo: anotherBuilder.
	]