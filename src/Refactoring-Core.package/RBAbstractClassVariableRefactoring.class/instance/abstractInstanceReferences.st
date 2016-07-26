transforming
abstractInstanceReferences
	| replacer |
	replacer := RBParseTreeRewriter 
		variable: variableName
		getter: 'class ' , self accessorsRefactoring getterMethod
		setter: 'class ' , self accessorsRefactoring setterMethod.
	self 
		convertClasses: class withAllSubclasses
		select: [ :aClass | aClass whichSelectorsReferToClassVariable: variableName ]
		using: replacer