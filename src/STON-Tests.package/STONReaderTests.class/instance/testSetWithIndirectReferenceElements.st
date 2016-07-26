tests
testSetWithIndirectReferenceElements
	| elementsCollection set ston object |
	elementsCollection := OrderedCollection streamContents: [ :out |
		10 timesRepeat: [ out nextPut: UUID new ] ].
	set := Set withAll: (elementsCollection collect: [ :each | Array with: each ]).
	object := Array with: elementsCollection with: set.
	ston := STON toStringPretty: object.
	object := STON fromString: ston readStream.
	object first do: [ :each |
		self assert: (object second includes: (Array with: each)) ].
	self assert: object second isHealthy 