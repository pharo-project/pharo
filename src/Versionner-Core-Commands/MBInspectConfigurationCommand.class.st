"
Command to inspect a configuration in versionner
"
Class {
	#name : #MBInspectConfigurationCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #documentation }
MBInspectConfigurationCommand >> documentation [
	^ 'Open a Smalltalk inspector on the Configuration.'
]

{ #category : #execute }
MBInspectConfigurationCommand >> execute [
	target inspect
]

{ #category : #accessing }
MBInspectConfigurationCommand >> title [
	^ 'inspect'
]
