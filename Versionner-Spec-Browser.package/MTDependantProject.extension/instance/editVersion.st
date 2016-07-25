*Versionner-Spec-Browser
editVersion
	| availableVersions  index |
	
	configurationClass ifNil: [ UIManager inform: 'Cannot find the configuration class for ' , name.
							^ self ].
	availableVersions := configurationClass project versions 
							select: [ :aVersion | aVersion blessing ~= #baseline ]
							thenCollect: [ :aVersion | aVersion versionString ].
	availableVersions := configurationClass project symbolicVersionSymbols , availableVersions.
	
	index := UIManager default 
		chooseFrom:  availableVersions
		title: 'Available versions in ', name.
	(index = 0)
		ifFalse: [ | versionString |
				versionString := availableVersions at: index.
				self version: (MTVersion fromVersionString:  versionString) ]