private
categoriesFor: anEnvironment 
	| allCategories |
	allCategories := IdentitySet withAll: universalEnvironment categories.
	allCategories removeAll: anEnvironment categories.
	anEnvironment not categories 
		do: [ :each | allCategories remove: each ifAbsent: [ ] ].
	allCategories 
		do: [ :each | self assert: (universalEnvironment classNamesFor: each) isEmpty ]