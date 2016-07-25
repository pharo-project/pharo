transforming
checkSuperMessages
	self inlineClass = class ifTrue: [^self].
	self inlineClass superclass isNil ifTrue: [^self].
	inlineParseTree superMessages do: 
			[:each | 
			(self inlineClass superclass whoDefinesMethod: each) 
				= (class superclass whoDefinesMethod: each) 
					ifFalse: 
						[self 
							refactoringError: ('Cannot inline method since it sends a super message <1s> that is overriden' 
									expandMacrosWith: each)]]