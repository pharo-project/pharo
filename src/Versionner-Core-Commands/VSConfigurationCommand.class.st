"
A command that apply to a project configuration.
"
Class {
	#name : #VSConfigurationCommand,
	#superclass : #VSCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #accessing }
VSConfigurationCommand >> configurationClass [
	^ target configurationClass
]
