"
Command to load a configuration in Versionner
"
Class {
	#name : #MBLoadConfigurationCommand,
	#superclass : #MBDevCycleCommands,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
MBLoadConfigurationCommand >> askForProjectName [
	^ self pickRepositoryFrom: self repositories title: 'Choose the repository where project is located'
]

{ #category : #documentation }
MBLoadConfigurationCommand >> documentation [
	^ 'Load configuration from a repository'
]

{ #category : #execute }
MBLoadConfigurationCommand >> execute [
	| projectRepository configRef configRefMap configNames configName |
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
]

{ #category : #ordering }
MBLoadConfigurationCommand >> order [

	^'10'
]

{ #category : #accessing }
MBLoadConfigurationCommand >> title [
	^ 'load configuration'
]
