transforming
pushDown: aSelector 
	| code protocols refactoring |
	code := class sourceCodeFor: aSelector.
	protocols := class protocolsFor: aSelector.
	refactoring := RBExpandReferencedPoolsRefactoring 
				model: self model
				forMethod: (class parseTreeFor: aSelector)
				fromClass: class
				toClasses: class subclasses.
	self performComponentRefactoring: refactoring.
	class subclasses do: 
			[:each | 
			(each directlyDefinesMethod: aSelector) 
				ifFalse: [each compile: code classified: protocols]]