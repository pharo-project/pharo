accessing
classesDo: aBlock
	| enumerated |
	enumerated := IdentitySet new.
	environment classesDo: [ :each | 
		aBlock value: each.
		enumerated add: each].
	orEnvironment classesDo: [ :each | 
		(enumerated includes: each) 
			ifFalse: [ aBlock value: each ] ]