transforming
abstractClassReferences
	| replacer |
	replacer := RBParseTreeRewriter 
		variable: variableName
		getter: self accessorsRefactoring getterMethod
		setter: self accessorsRefactoring setterMethod.
	self 
		convertClasses: class theMetaClass withAllSubclasses
		select: 
			[ :aClass | 
			(aClass whichSelectorsReferToClassVariable: variableName) reject: 
				[ :each | 
				aClass == class theMetaClass and: 
					[ each == self accessorsRefactoring getterMethod or: [ each == self accessorsRefactoring setterMethod ] ] ] ]
		using: replacer