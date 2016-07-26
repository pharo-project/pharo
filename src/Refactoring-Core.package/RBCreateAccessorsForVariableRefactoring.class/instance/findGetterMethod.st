private-accessing
findGetterMethod
	| definingClass matcher |
	definingClass := self definingClass.
	matcher := RBParseTreeSearcher getterMethod: variableName.
	^self possibleGetterSelectors detect: 
			[:each | 
			(self 
				checkClass: definingClass
				selector: each
				using: matcher) notNil 
				and: [(definingClass subclassRedefines: each) not]]
		ifNone: [nil]