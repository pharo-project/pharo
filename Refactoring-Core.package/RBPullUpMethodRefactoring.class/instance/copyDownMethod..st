private
copyDownMethod: aSelector 
	| oldProtocol oldSource superclassDefiner subclasses refactoring |
	superclassDefiner := class superclass whoDefinesMethod: aSelector.
	superclassDefiner isNil ifTrue: [^self].
	oldSource := superclassDefiner sourceCodeFor: aSelector.
	oldSource isNil 
		ifTrue: 
			[self 
				refactoringFailure: ('Source code for <1s> superclass method not available' 
						expandMacrosWith: aSelector)].
	oldProtocol := superclassDefiner protocolsFor: aSelector.
	subclasses := class superclass subclasses 
				reject: [:each | each directlyDefinesMethod: aSelector].
	subclasses isEmpty ifTrue: [^self].
	((superclassDefiner parseTreeFor: aSelector) superMessages 
		detect: [:each | superclassDefiner directlyDefinesMethod: each]
		ifNone: [nil]) notNil 
		ifTrue: 
			[self 
				refactoringError: ('Cannot pull up <1s> since we must copy down the superclass method in <2p><n>to the other subclasses, and the superclass method sends a super message which is overriden.' 
						expandMacrosWith: aSelector
						with: superclassDefiner)].
	self 
		refactoringWarning: 'Do you want to copy down the superclass method to the classes that don''t define ' 
				, aSelector.
	refactoring := RBExpandReferencedPoolsRefactoring 
				model: self model
				forMethod: (superclassDefiner parseTreeFor: aSelector)
				fromClass: superclassDefiner
				toClasses: subclasses.
	self performComponentRefactoring: refactoring.
	subclasses do: [:each | each compile: oldSource classified: oldProtocol]