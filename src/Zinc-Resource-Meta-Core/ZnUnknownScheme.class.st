"
ZnUnknownScheme is signalled when an unknown/unsupported URL scheme is found.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnUnknownScheme,
	#superclass : #Error,
	#instVars : [
		'scheme'
	],
	#category : #Zinc-Resource-Meta-Core
}

{ #category : #'instance creation' }
ZnUnknownScheme class >> scheme: scheme [
	^ self new
		scheme: scheme;
		yourself
]

{ #category : #accessing }
ZnUnknownScheme >> scheme [
	^ scheme
]

{ #category : #accessing }
ZnUnknownScheme >> scheme: anObject [
	scheme := anObject
]
