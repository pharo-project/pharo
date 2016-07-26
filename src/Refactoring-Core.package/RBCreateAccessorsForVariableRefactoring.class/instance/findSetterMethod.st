private-accessing
findSetterMethod
	| definingClass matcher |
	definingClass := self definingClass.
	matcher := self needsReturnForSetter 
				ifTrue: [RBParseTreeSearcher returnSetterMethod: variableName]
				ifFalse: [RBParseTreeSearcher setterMethod: variableName].
	^self possibleSetterSelectors detect: 
			[:each | 
			(self 
				checkClass: definingClass
				selector: each
				using: matcher) notNil 
				and: [(definingClass subclassRedefines: each) not]]
		ifNone: [nil]