"
I represent a project that cannot be read from the history.
I fetch most of my properties from the current project that may be a project or an unborn project.
"
Class {
	#name : #IceInvalidProject,
	#superclass : #IceAbstractProject,
	#category : #'Iceberg-Project'
}

{ #category : #testing }
IceInvalidProject >> isInvalid [
	
	^ true
]

{ #category : #accessing }
IceInvalidProject >> sourceDirectory [
	
	"I'm an invalid project.
	Ask the current project as a fallback"
	^ self repository project sourceDirectory
]
