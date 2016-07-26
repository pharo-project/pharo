tests - iterate
testDetectIfFoundIfNoneWhenSomethingIsFound
	| wasFound foundObject |
	foundObject := nil.
	wasFound := self collectionWithoutNilElements
		detect: [ :each | each notNil ]
		ifFound: [ :element | 
			foundObject := element.
			true ]
		ifNone: [ false ].
	self
		assert: wasFound;
		assert: (self collectionWithoutNilElements includes: foundObject)