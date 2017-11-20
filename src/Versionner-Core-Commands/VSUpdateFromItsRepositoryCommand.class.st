"
Update the configuration from the version contained in its repository.
"
Class {
	#name : #VSUpdateFromItsRepositoryCommand,
	#superclass : #VSConfigurationCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #execute }
VSUpdateFromItsRepositoryCommand >> execute [
	"use the Metacello api"

	target project updateProject.
	
]

{ #category : #accessing }
VSUpdateFromItsRepositoryCommand >> title [
	^ 'update from its repository'
]
