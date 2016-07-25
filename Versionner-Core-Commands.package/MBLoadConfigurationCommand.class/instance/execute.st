execute
execute
	| projectRepository configRef configurationInfo versionInfos versionInfo version configRefMap configNames configName |
	projectRepository := self askForProjectName.
	projectRepository ifNil: [ ^ self ].
	configRefMap := self configurationReferencesFrom: projectRepository.
	
	configRefMap isEmpty
		ifTrue: [ ^ self inform: 'No configurations found in the project repository ' , projectRepository description printString ].
	configNames := configRefMap keys asSortedCollection asArray.
	configName := self uiManager 
		chooseFrom: (configNames collect: [ :name | name configurationBaseName])
		values: configNames
		title: 'Select the desired project'.
	configName ifNil: [ ^ self ].
	
	configRef := (configRefMap at: configName) asSortedCollection asArray last.
	Gofer new
		repository: projectRepository;
		version: configRef name;
		load