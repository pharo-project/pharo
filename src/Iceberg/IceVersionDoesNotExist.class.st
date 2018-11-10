"
Version does not exist, probably the package was created in a later commit or it was deleted.
"
Class {
	#name : #IceVersionDoesNotExist,
	#superclass : #IceError,
	#instVars : [
		'version'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #accessing }
IceVersionDoesNotExist >> messageText [
	^ 'There is no version for package: {1} in  commit {2}' 
		format: { 
			self version packageName.
			self version commit id
		}
]

{ #category : #accessing }
IceVersionDoesNotExist >> version [
	^ version
]

{ #category : #accessing }
IceVersionDoesNotExist >> version: anObject [
	version := anObject
]
