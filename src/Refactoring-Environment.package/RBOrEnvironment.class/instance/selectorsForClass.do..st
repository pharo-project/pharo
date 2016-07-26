accessing
selectorsForClass: aClass do: aBlock
	| enumerated |
	enumerated := IdentitySet new.
	environment
		selectorsForClass: aClass
		do: [ :each | 
			enumerated add: each.
			aBlock value: each ].
	orEnvironment
		selectorsForClass: aClass
		do: [:each | 
			(enumerated includes: each) 
				ifFalse: [ aBlock value: each ] ]