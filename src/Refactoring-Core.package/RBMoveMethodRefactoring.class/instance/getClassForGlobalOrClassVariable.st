private-accessing
getClassForGlobalOrClassVariable
	| definingClass type |
	definingClass := class whoDefinesClassVariable: (variable ifNil: ['']).
	definingClass ifNil: [ 
			type := self model classNamed: variable.
			type ifNil: [ type := self model classNamed: #Object ] ]
		ifNotNil: [ type := definingClass typeOfClassVariable: variable ].
	moveToClasses := self selectVariableTypesFrom: (Array with: type) selected: (Array with: type).
	moveToClasses ifNil: [ self refactoringFailure: 'Method not moved' ]