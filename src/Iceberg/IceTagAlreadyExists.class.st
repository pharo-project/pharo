Class {
	#name : #IceTagAlreadyExists,
	#superclass : #IceError,
	#instVars : [
		'tagName'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #accessing }
IceTagAlreadyExists >> messageText [

	^ 'Tag {1} already exists' format: { tagName }
]

{ #category : #accessing }
IceTagAlreadyExists >> tagName [
	^ tagName
]

{ #category : #accessing }
IceTagAlreadyExists >> tagName: anObject [
	tagName := anObject
]
