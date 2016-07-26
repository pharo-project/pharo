private-inst var
privateMoveInstVarToClass: aClass
	| newClass |
	newClass := (SearchFacade classSearchInEnvironment: RBBrowserEnvironment default) chooseFromOwner: World.
	^ self class: aClass andInstVariable: [ :class :variable | 
		RBMoveInstVarToClassRefactoring 
			model: environment
			variable: variable
			class: newClass
			oldClass: aClass ]