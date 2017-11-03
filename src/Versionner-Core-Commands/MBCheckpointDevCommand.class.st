"
Command for a check point in Versionner
"
Class {
	#name : #MBCheckpointDevCommand,
	#superclass : #MBDevCycleCommands,
	#category : #Versionner-Core-Commands
}

{ #category : #documentation }
MBCheckpointDevCommand >> documentation [
	^ 'Save modified packages in their respective repository, update the configuration and save a version of the configuration in it''s repository.'
]

{ #category : #execute }
MBCheckpointDevCommand >> execute [
	| message configClass |
	configClass := self configurationClass.
	message := self uiManager
		multilineRequest:
			'Enter commit message for ' , (configClass name configurationBaseName) , ' '
				, (target project version: #development) versionString
		initialAnswer: (target project version: #development) versionString, ' ():'.
	message == nil
		ifTrue: [ ^ self ].
	self executeWithMessage: message

]

{ #category : #execute }
MBCheckpointDevCommand >> executeWithMessage: message [
	| configClass |
	configClass := self configurationClass.
	self checkUserInRepositoryOfPackage: configClass name asString.
	MetacelloToolBox saveModifiedPackagesAndConfigurationIn: configClass description: message.

]

{ #category : #ordering }
MBCheckpointDevCommand >> order [

	^'50'
]

{ #category : #accessing }
MBCheckpointDevCommand >> title [
	^ 'checkpoint dev'
]
