"
Command to load a configuration in versionner
"
Class {
	#name : #MBSaveConfigurationCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #documentation }
MBSaveConfigurationCommand >> documentation [
	^ 'Save the configuration in its repository. Note that the dependents projects and defined packages are not saved. Use ''checkpoint dev'' command to save packages.'
]

{ #category : #execute }
MBSaveConfigurationCommand >> execute [
	| message |
	message := self uiManager multilineRequest: 'Enter commit message' initialAnswer: ''.
	message == nil
		ifTrue: [ ^ self ].
	self executeWithMessage: message
]

{ #category : #execute }
MBSaveConfigurationCommand >> executeWithMessage: message [
	"execute with message"
	self checkUserInRepositoryOfPackage: self configurationClass name asString.
	MetacelloToolBox 
		saveConfigurationPackageFor: self configurationClass name asString 
		description: message
]

{ #category : #accessing }
MBSaveConfigurationCommand >> title [
	^ 'save configuration'
]
