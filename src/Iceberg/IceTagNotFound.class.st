Class {
	#name : #IceTagNotFound,
	#superclass : #IceError,
	#instVars : [
		'tagName'
	],
	#category : #'Iceberg-Core'
}

{ #category : #accessing }
IceTagNotFound >> tagName: aString [ 
	tagName := aString
]
