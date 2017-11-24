"
ZnUnknownHttpVersion is signalled when an unknown/unsupported HTTP version is found.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnUnknownHttpVersion,
	#superclass : #ZnParseError,
	#instVars : [
		'version'
	],
	#category : #'Zinc-HTTP-Exceptions'
}

{ #category : #'instance creation' }
ZnUnknownHttpVersion class >> version: version [
	^ self new
		version: version;
		yourself
]

{ #category : #accessing }
ZnUnknownHttpVersion >> version [
	^ version
]

{ #category : #accessing }
ZnUnknownHttpVersion >> version: anObject [
	version := anObject
]
