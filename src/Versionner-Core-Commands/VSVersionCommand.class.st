"
A command that apply to a project version.
"
Class {
	#name : #VSVersionCommand,
	#superclass : #VSCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #accessing }
VSVersionCommand >> configurationClass [
	^ target configurationClass
]
