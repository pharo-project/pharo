Class {
	#name : #IceUnknownVersionWorkingCopy,
	#superclass : #IceWorkingCopyState,
	#category : #'Iceberg-WorkingCopy'
}

{ #category : #testing }
IceUnknownVersionWorkingCopy >> description [
	
	^ 'Fetch required. ', self referenceCommit description
]

{ #category : #testing }
IceUnknownVersionWorkingCopy >> isDetached [
	
	^ true
]

{ #category : #testing }
IceUnknownVersionWorkingCopy >> isUnknownCommitState [
	
	^ true
]
