notifications
recategorize: aClass to: newCategory
	| oldCategory |
	
	oldCategory := aClass category.
	oldCategory asSymbol == newCategory asSymbol
		ifTrue: [ ^ self ].
		
	environment organization 
		classify: aClass name 
		under: newCategory.
		
	self systemAnnouncer 
		class: aClass 
		recategorizedFrom: oldCategory 
		to: newCategory