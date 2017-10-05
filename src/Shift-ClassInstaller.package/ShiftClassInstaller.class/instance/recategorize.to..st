notifications
recategorize: aClass to: newCategory
	| oldCategory |
	oldCategory := oldClass ifNotNil:[oldClass basicCategory].
	oldCategory == newCategory asSymbol
		ifTrue: [ ^ self ].

	builder environment organization ifNotNil: [ :e | e classify: aClass name under: newCategory ].

	(oldCategory isNil or:[ oldCategory = #Unclassified])
		ifTrue: [ SystemAnnouncer uniqueInstance classAdded: aClass inCategory: newCategory ]
		ifFalse: [ SystemAnnouncer uniqueInstance class: aClass recategorizedFrom: oldCategory to: newCategory ]