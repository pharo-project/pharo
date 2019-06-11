"
Browse local changes
"
Class {
	#name : #GoferBrowseLocalChanges,
	#superclass : #GoferLocalChanges,
	#category : #'Gofer-UI'
}

{ #category : #deprecation }
GoferBrowseLocalChanges class >> isDeprecated [
	"I am unused in the system. I will be remove in Pharo 9."

	^ true
]

{ #category : #running }
GoferBrowseLocalChanges >> execute [
	^ super execute browse
]
