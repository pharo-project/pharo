"
Command to validate all configurations in versionner
"
Class {
	#name : #MBValidateAllConfigurationCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #documentation }
MBValidateAllConfigurationCommand >> documentation [
	^ 'Validate all configuration.'
]

{ #category : #execute }
MBValidateAllConfigurationCommand >> execute [
	self showExecuteWhile: [ target configurationRoot configurations doWithIndex: [ :info :index | info validateFull ] ]
]

{ #category : #accessing }
MBValidateAllConfigurationCommand >> title [
	^ 'validate all configurations'
]
