comparing
= anObject
	^ anObject class = self class and: [ enclosedElement = anObject enclosedSetElement ]
