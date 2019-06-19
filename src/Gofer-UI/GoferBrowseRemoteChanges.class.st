"
Browse remote changes
"
Class {
	#name : #GoferBrowseRemoteChanges,
	#superclass : #GoferRemoteChanges,
	#category : #'Gofer-UI'
}

{ #category : #deprecation }
GoferBrowseRemoteChanges class >> isDeprecated [
	"I am unused in the system. I will be remove in Pharo 9."

	^ true
]

{ #category : #running }
GoferBrowseRemoteChanges >> execute [
	^ super execute browse
]
