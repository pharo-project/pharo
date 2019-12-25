"
I associate a piece of data that should be delivered to a remote server with the url of that remote server.

Instances of me should be sent to a GTEventDelivery.


"
Class {
	#name : #GTEventBundle,
	#superclass : #Object,
	#instVars : [
		'url',
		'category',
		'data'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #visitor }
GTEventBundle >> accept: aVisitor [ 
	^ aVisitor visitSingleBundle: self
]

{ #category : #accessing }
GTEventBundle >> category [
	^ category
]

{ #category : #accessing }
GTEventBundle >> category: anObject [
	category := anObject
]

{ #category : #accessing }
GTEventBundle >> data [
	^ data
]

{ #category : #accessing }
GTEventBundle >> data: anObject [
	data := anObject
]

{ #category : #'accessing - computed' }
GTEventBundle >> entity [
	^ ZnByteArrayEntity bytes: self data
]

{ #category : #accessing }
GTEventBundle >> url [
	^ url
]

{ #category : #accessing }
GTEventBundle >> url: anObject [
	url := anObject
]
