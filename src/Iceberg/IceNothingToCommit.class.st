"
I indicate when there is nothing to commit (and you tried to do it)
"
Class {
	#name : #IceNothingToCommit,
	#superclass : #IceError,
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceNothingToCommit >> acceptError: anObject [
	anObject visitNothingToCommitError: self
]
