"
Unload the package with the selected configuration.
"
Class {
	#name : #VSUnloadConfigurationCommand,
	#superclass : #VSConfigurationCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
VSUnloadConfigurationCommand >> execute [
	| answer |
	answer := UIManager default confirm: 'Are you sure you want to unload ' , target name.
	answer
		ifTrue: [ target workingCopy unload ]
]

{ #category : #accessing }
VSUnloadConfigurationCommand >> title [
	^ 'unload'
]
