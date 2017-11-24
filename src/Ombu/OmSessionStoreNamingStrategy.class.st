"
My subclasses define a way to find a name for a new Ombu log.  Names should not collide with an existing file in the same directory.
"
Class {
	#name : #OmSessionStoreNamingStrategy,
	#superclass : #Object,
	#category : #'Ombu-Strategies'
}

{ #category : #accessing }
OmSessionStoreNamingStrategy class >> availableStrategies [

	^ self allSubclasses reject: #hasAbstractMethods
]

{ #category : #accessing }
OmSessionStoreNamingStrategy >> nextTo: aName in: aDirectory [
	"Answer a name for a new Ombu log, that should not collide with an existing file in the same directory."
	
	^ self subclassResponsibility
]
