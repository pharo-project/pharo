running
basicCheck: aMethod 
	(self matcher canMatchMethod: aMethod) 
		ifFalse: [ ^ false ].
	^ (self matcher executeTree: aMethod ast initialAnswer: nil) isNotNil