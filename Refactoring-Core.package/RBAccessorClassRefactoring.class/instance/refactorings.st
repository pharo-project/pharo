accessing
refactorings
	| class |
	class := self classObjectFor: className asSymbol.
	^  class instanceVariableNames collect: [ :each | 
		RBCreateAccessorsForVariableRefactoring
			variable: each class: class classVariable: false ]