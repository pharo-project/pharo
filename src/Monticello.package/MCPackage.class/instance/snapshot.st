accessing
snapshot
	| rPackageSet definitions categories |
	rPackageSet := self packageSet.
	definitions := OrderedCollection new.
	categories := rPackageSet categoryNames asArray.
	categories isEmpty 
		ifFalse: [ definitions add: (MCOrganizationDefinition categories: categories) ].
		
	rPackageSet methods 
		do: [:ea | definitions add: ea asMCMethodDefinition] 
		displayingProgress: [ :ea| 'Snapshotting methods...' ].
		
	rPackageSet overriddenMethods
		do: [:ea | definitions add:
					(rPackageSet changeRecordForOverriddenMethod: ea) asMCMethodDefinition]
		displayingProgress: [ :ea| 'Searching for overrides in ', ea asString ].
		
	rPackageSet definedClasses 
		do: [:ea | definitions addAll: ea classDefinitions] 
		displayingProgress: [ :ea| 'Snapshotting class ', ea asString ].
		
	^ MCSnapshot fromDefinitions: definitions
