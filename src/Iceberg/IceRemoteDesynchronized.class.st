"
I indicate when a remote is desyncronized. 
It means the version I have in the working copy (not the image) is different to the version I have in the remote (then I need to pull/branch/whatever before commiting).
"
Class {
	#name : #IceRemoteDesynchronized,
	#superclass : #IceError,
	#instVars : [
		'remote',
		'resumable'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceRemoteDesynchronized >> acceptError: aVisitor [
	aVisitor visitRemoteDesynchronizedError: self
]

{ #category : #converting }
IceRemoteDesynchronized >> asResumable [
	resumable := true
]

{ #category : #initialization }
IceRemoteDesynchronized >> initialize [
	super initialize.
	resumable := false
]

{ #category : #private }
IceRemoteDesynchronized >> isResumable [
	^ resumable
]

{ #category : #accessing }
IceRemoteDesynchronized >> remote [
	^ remote
]

{ #category : #accessing }
IceRemoteDesynchronized >> remote: anObject [
	remote := anObject
]
