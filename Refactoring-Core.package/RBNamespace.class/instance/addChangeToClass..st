private-changes
addChangeToClass: aRBClass 
	^ changedClasses 
		at: aRBClass name
		put: (Array 
				with: aRBClass theNonMetaClass
				with: aRBClass theMetaClass)