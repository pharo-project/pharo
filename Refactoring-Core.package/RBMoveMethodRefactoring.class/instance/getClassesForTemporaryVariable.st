private-accessing
getClassesForTemporaryVariable
	| types |
	types := RBRefactoryTyper 
				typesFor: variable
				in: parseTree
				model: self model.
	types isEmpty 
		ifTrue: [types := OrderedCollection with: (self model classNamed: #Object)].
	moveToClasses := self selectVariableTypesFrom: types selected: types.
	moveToClasses isNil ifTrue: [self refactoringFailure: 'Method not moved']