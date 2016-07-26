transforming
pullUp: aSelector 
	| source refactoring |
	source := class sourceCodeFor: aSelector.
	source isNil 
		ifTrue: [self refactoringFailure: 'Source for method not available'].
	refactoring := RBExpandReferencedPoolsRefactoring 
				model: self model
				forMethod: (class parseTreeFor: aSelector)
				fromClass: class
				toClasses: (Array with: class superclass).
	self performComponentRefactoring: refactoring.
	class superclass compile: source
		classified: (class protocolsFor: aSelector)