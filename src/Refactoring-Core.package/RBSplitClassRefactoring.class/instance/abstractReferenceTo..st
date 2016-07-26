private-transforming
abstractReferenceTo: each 
	| setterMethod replacer accessorRef getterMethod |
	accessorRef := RBCreateAccessorsForVariableRefactoring 
				variable: each
				class: newClass
				classVariable: false.
	self performComponentRefactoring: accessorRef.
	getterMethod := accessorRef getterMethod.
	setterMethod := accessorRef setterMethod.
	replacer := RBParseTreeRewriter 
				variable: each
				getter: getterMethod
				setter: setterMethod
				receiver: referenceVariableName.
	self 
		convertClasses: class withAllSubclasses
		select: [:aClass | aClass whichSelectorsReferToInstanceVariable: each]
		using: replacer.
	self performComponentRefactoring: (RBRemoveInstanceVariableRefactoring 
				remove: each
				from: class)