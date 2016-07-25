comparing
= anObject 

	^super = anObject and: [ 
		self targetClassName = anObject targetClassName and: [ 
			inverseName = anObject inverseName ] ]

	