printing
printTestSelectorOn: aStream
	testSelector
		ifNotNil: [ 
			| class |
			class := self class whichClassIncludesSelector: testSelector.
			class ~= self class
				ifTrue: [ self printTestSelectorDefiningClass: class on: aStream ].
			aStream
				nextPutAll: '>>#';
				nextPutAll: testSelector ]