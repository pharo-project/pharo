tests - copy
testCopyNonEmptyWithout
	"self debug: #testCopyNonEmptyWithout"
	
	| res anElementOfTheCollection |
	anElementOfTheCollection :=  self nonEmpty anyOne.
	res := (self nonEmpty copyWithout: anElementOfTheCollection).
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self deny: (res includes: anElementOfTheCollection).
	self nonEmpty do:
		[:each | (each = anElementOfTheCollection) 
					ifFalse: [self assert: (res includes: each)]].
	
