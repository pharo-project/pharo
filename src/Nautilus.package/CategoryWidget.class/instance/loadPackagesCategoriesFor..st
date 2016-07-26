private
loadPackagesCategoriesFor: class
	| env |
	
	env :=  self model browsedEnvironment.
	^env isSystem
		ifTrue: [ self loadPackagesCategoriesInSystemEnvironmentFor: class ]
		ifFalse: [ self loadPackagesCategoriesInARestrictedEnvironment: env for: class  ]