private-method
privateDeprecateMethodFor: aMethod
	| class selector newMethod |
	class := aMethod methodClass.
	selector := aMethod selector.
	newMethod := (SearchFacade messageSearchFor: aMethod methodClass) chooseFromOwner: self model window.
	^ RBDeprecateMethodRefactoring
		model: environment
		deprecateMethod: selector
		in: class
		using: newMethod selector