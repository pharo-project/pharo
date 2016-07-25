text areas behavior
sourceCode

	^ self selectedClass 
		ifNil: [
			self isExamplesSymbol
				ifTrue: [ self class methodFinderExplanation ]
				ifFalse: [ self defaultExplanation]]
		ifNotNil:[
			self selectedMethod 
				ifNil: [
					self buildDescriptionOf: self selectedClass]
				ifNotNil:[
					| method |
					method := self isExamplesSymbol
								ifTrue: [self finder findSelector: self selectedMethod]
								ifFalse: [self selectedMethod].
					(self selectedClass >> method) sourceCode]]