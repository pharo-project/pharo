"
I represent a project registration in iceberg. I adapt the behavior of metacello project registrations, containing a version.

For the moment I'm only used for compatibility reasons.
"
Class {
	#name : #IceMetacelloProjectRegistration,
	#superclass : #Object,
	#instVars : [
		'version'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #printing }
IceMetacelloProjectRegistration >> locked [
	"Only for compatibility for now"
	^ false
]

{ #category : #printing }
IceMetacelloProjectRegistration >> printOn: aStream [

	aStream nextPutAll: version asString
]

{ #category : #accessing }
IceMetacelloProjectRegistration >> version: aString [ 
	version := aString
]
