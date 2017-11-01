"
I am ZnValueDelegate.
I forward #handleRequest: messages to the object that I wrap using #value:

ZnServer default delegate: 
	(ZnValueDelegate with: [ :request | 
		ZnResponse ok: (ZnEntity with: 'You asked for ', request uri printString) ] ).

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnValueDelegate,
	#superclass : #Object,
	#instVars : [
		'object'
	],
	#category : 'Zinc-HTTP-Client-Server'
}

{ #category : #'instance creation' }
ZnValueDelegate class >> with: object [
	^ self new
		object: object;
		yourself
]

{ #category : #public }
ZnValueDelegate >> handleRequest: request [
	^ object value: request
]

{ #category : #initialize-release }
ZnValueDelegate >> object: anObject [
	object := anObject
]
