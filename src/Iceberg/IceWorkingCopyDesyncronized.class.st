"
I indicate when a working copy is desyncronized. 
It means the version I have in the image is different to the version I have in the working copy.
"
Class {
	#name : #IceWorkingCopyDesyncronized,
	#superclass : #IceError,
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceWorkingCopyDesyncronized >> acceptError: anObject [
	anObject visitWorkingCopyDesynchronizedError: self
]
