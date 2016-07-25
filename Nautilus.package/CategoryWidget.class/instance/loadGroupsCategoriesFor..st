private
loadGroupsCategoriesFor: class
	| env |
	
	env := model browsedEnvironment.
	^ env isSystem
		ifTrue: [ self loadGroupsCategoriesInSytemEnvironmentFor: class ]
		ifFalse: [ self loadGroupsCategoriesInARestrictedEnvironment: env for: class ]