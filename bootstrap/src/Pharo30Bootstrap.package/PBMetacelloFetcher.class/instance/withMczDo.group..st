actions
withMczDo: aBlock group: aGroup

	| metacelloVersion record |
	metacelloVersion := ConfigurationOfPharoBootstrap project version: version.
	record := 	[ (metacelloVersion ignoreImage: true) record: aGroup ] 
		on: MCMergeOrLoadWarning do: [ :e | 
			"we merge" e resume: false ].
	record loadDirective packageDirectivesDo: [:directive | | repository mcVersion |
		
		repository := (record repositoryMap 
			at: directive externalReference packageName
			ifAbsent: [ record repositoryMap 
										at: directive externalReference name ]).
		repository := repository isCollection
			ifTrue: [ repository anyOne ]
			ifFalse: [ repository ].
			
		mcVersion := repository versionFromFileNamed: directive file , '.mcz'.
		aBlock value: mcVersion
	].