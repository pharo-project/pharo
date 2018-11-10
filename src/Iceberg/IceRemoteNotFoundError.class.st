Class {
	#name : #IceRemoteNotFoundError,
	#superclass : #IceError,
	#instVars : [
		'remoteName'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #accessing }
IceRemoteNotFoundError >> remoteName [
	^ remoteName
]

{ #category : #accessing }
IceRemoteNotFoundError >> remoteName: aString [ 
	remoteName := aString
]
