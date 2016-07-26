tests
testReadBeforeWritten1
	self 
		assert: (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: (RBParser 
						parseMethod: 'addAll: aCollection 
	"Answer aCollection, having added all elements
	 of aCollection to the receiver.

	 Fail if aCollection is not a kind of
	 Collection."

	| newSize elementsSize growSize |
	(newSize := aCollection size * 2) > elements size ifTrue: [self rehash: newSize].
	elementsSize := elements size.
	growSize := elementsSize // 2.
	aCollection do: 
			[:newObject | 
			| hashIndex element |
			newObject == nil ifFalse: 
					[hashIndex := self hashIndexFor: newObject.
					
					[(element := elements at: hashIndex) == nil
						ifTrue: 
							[elements at: hashIndex put: newObject.
							(elementCount := elementCount + 1) > growSize ifTrue: 
									[self expand.
									elementsSize := elements size.
									growSize := elementsSize // 2].
							true]
						ifFalse: [element == newObject]]
							whileFalse: 
								[(hashIndex := hashIndex + 1) > elementsSize ifTrue: [hashIndex := 1]]]].
	^aCollection')) 
				isEmpty