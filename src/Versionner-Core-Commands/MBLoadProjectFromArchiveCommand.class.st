"
Command to load project from archive in versionner
"
Class {
	#name : #MBLoadProjectFromArchiveCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #documentation }
MBLoadProjectFromArchiveCommand >> documentation [ 
	^ 'Load project from an archive.'
]

{ #category : #execute }
MBLoadProjectFromArchiveCommand >> execute [
	| archiveRepository configRef configurationInfo versionInfos versionInfo version configRefMap configNames configName |
	archiveRepository := self pickRepositoryFrom: self repositories title: 'Choose the archive repository'.
	archiveRepository ifNil: [ ^ self ].
	configRefMap := self configurationReferencesFrom: archiveRepository.
	configRefMap isEmpty
		ifTrue: [ ^ self inform: 'No configurations found in the archive repository ' , archiveRepository description printString ].
	configNames := configRefMap keys.
	configName := self uiManager
		chooseFrom: (configNames collect: [ :name | name configurationBaseName])
		values: configNames
		title: 'Select the desired project'.
	configName ifNil: [ ^ self ].
	configRef := (configRefMap at: configName) asSortedCollection asArray last.
	(Gofer new)
		repository: archiveRepository;
		version: configRef name;
		load.
	configurationInfo := (MBConfigurationInfo configurationClass: (Smalltalk globals at: configRef packageName asSymbol))
		configurationRoot: MBConfigurationRoot new;
		trimVersionLists: false;
		currentVersion: #notLoaded;
		yourself.
	versionInfos := configurationInfo versions.
	versionInfo := self uiManager
		chooseFrom: (versionInfos collect: [ :vrsn | vrsn printString ])
		values: versionInfos
		title: 'Select the desired version'.
	versionInfo ifNil: [ ^ self ].
	version := versionInfo version.
	version repositoryOverrides: {archiveRepository}.
	version load: version defaultPackageNamesToLoad
]

{ #category : #ordering }
MBLoadProjectFromArchiveCommand >> order [

	^'01'
]

{ #category : #accessing }
MBLoadProjectFromArchiveCommand >> title [
	^ 'load archive version'
]
