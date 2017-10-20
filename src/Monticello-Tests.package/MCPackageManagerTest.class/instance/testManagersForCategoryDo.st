running
testManagersForCategoryDo
	"Consider the following package structure:
		Renraku
		Renraku-Help

		and a method extension with the protocol '*renraku'.
		MCPackageManager>>methodModified: should only mark 'Renraku' as modified,
		not 'Renraku-Help'"
	| managers |
	self assert: (MCPackageManager registry includesKey: mcPackage1).
	self assert: (MCPackageManager registry includesKey: mcPackage2).
	
	managers := OrderedCollection new.
	MCPackageManager
		managersForCategory: mcPackage1 name
		do: [ :manager | managers add: manager ].
		
	self assert: managers size equals: 1.
	self assert: managers first package == mcPackage1.
	
	managers := OrderedCollection new.
	MCPackageManager
		managersForCategory: mcPackage2 name
		do: [ :manager | managers add: manager ].
		
	self assert: managers size equals: 1.
	self assert: managers first package == mcPackage2