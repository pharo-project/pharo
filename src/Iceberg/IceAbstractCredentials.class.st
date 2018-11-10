"
I represent the common behavior of all iceberg credentials.
"
Class {
	#name : #IceAbstractCredentials,
	#superclass : #Object,
	#instVars : [
		'host'
	],
	#category : #'Iceberg-Security'
}

{ #category : #accessing }
IceAbstractCredentials >> host [
	^ host
]

{ #category : #accessing }
IceAbstractCredentials >> host: anObject [
	host := anObject
]

{ #category : #testing }
IceAbstractCredentials >> isDefault [
	^ false
]
