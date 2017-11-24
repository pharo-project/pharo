"
Browse the configuration class.
"
Class {
	#name : #VSBrowseConfigurationCommand,
	#superclass : #VSConfigurationCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
VSBrowseConfigurationCommand >> execute [
	self configurationClass browse
]

{ #category : #accessing }
VSBrowseConfigurationCommand >> title [
	^ 'browse configuration'
]
