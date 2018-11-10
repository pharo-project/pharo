"
I'm thrown when an attempt to pull was made and there are changes in the image. 
This is a resumable error because user may want to do it anyway.
"
Class {
	#name : #IceShouldCommitBeforePull,
	#superclass : #IceError,
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceShouldCommitBeforePull >> acceptError: anObject [
	anObject visitShouldCommitBeforePullError: self
]

{ #category : #private }
IceShouldCommitBeforePull >> isResumable [
	^ true
]
