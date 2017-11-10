"
This command allows you to check inconsistencies in a Configuration class.
"
Class {
	#name : #VSValidateConfigurationCommand,
	#superclass : #VSConfigurationCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #execute }
VSValidateConfigurationCommand >> execute [

	| result |
	result := self validate.
	result ifEmpty: [ UIManager default inform: 'The configuration is correctly defined'. ^ self ].

	UIManager default inform: 'Issues were found'.
	result := result collect: [ :each | each explanation ].
	result inspect
]

{ #category : #accessing }
VSValidateConfigurationCommand >> title [
	^ 'validate'
]

{ #category : #execute }
VSValidateConfigurationCommand >> validate [
	self showExecuteWhile: [ ^ target validateFull ]
]
