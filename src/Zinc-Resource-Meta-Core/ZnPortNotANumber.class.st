"
ZnPortNotANumber is signalled when an URL is parsed containing an illegal port specification.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnPortNotANumber,
	#superclass : #Error,
	#instVars : [
		'port'
	],
	#category : 'Zinc-Resource-Meta-Core'
}

{ #category : #'instance creation' }
ZnPortNotANumber class >> port: port [
	^ self new
		port: port;
		yourself
]

{ #category : #accessing }
ZnPortNotANumber >> port [
	^ port
]

{ #category : #accessing }
ZnPortNotANumber >> port: anObject [
	port := anObject
]
