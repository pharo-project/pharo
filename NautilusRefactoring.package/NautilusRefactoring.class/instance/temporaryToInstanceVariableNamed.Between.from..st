source
temporaryToInstanceVariableNamed: name Between: anInterval from: aMethod
	| refactoring | 
	aMethod ifNil: [ ^ self ].
	refactoring := self privateTemporaryToInstanceVariableNamed: name Between: anInterval from: aMethod.
	self performRefactoring: refactoring