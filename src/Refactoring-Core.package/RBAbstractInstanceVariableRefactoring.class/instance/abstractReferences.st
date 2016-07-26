transforming
abstractReferences
	| replacer |
	replacer := RBParseTreeRewriter 
				variable: variableName
				getter: self accessorsRefactoring getterMethod
				setter: self accessorsRefactoring setterMethod.
	self 
		convertClasses: class withAllSubclasses
		select: 
			[:aClass | 
			(aClass whichSelectorsReferToInstanceVariable: variableName) reject: 
					[:each | 
					aClass == class and: 
							[each == self accessorsRefactoring getterMethod 
								or: [each == self accessorsRefactoring setterMethod]]]]
		using: replacer