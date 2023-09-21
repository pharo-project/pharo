"
A GoferRepositoryError is the error thrown when a repository cannot be accessed.
"
Class {
	#name : 'GoferRepositoryError',
	#superclass : 'Error',
	#instVars : [
		'repository'
	],
	#category : 'Gofer-Core-Exceptions',
	#package : 'Gofer-Core',
	#tag : 'Exceptions'
}

{ #category : 'instance creation' }
GoferRepositoryError class >> signal: aString repository: aRepository [

	^ self new
		  repository: aRepository;
		  signal: aString
]

{ #category : 'private - testing' }
GoferRepositoryError >> isResumable [

	^ true
]

{ #category : 'accessing' }
GoferRepositoryError >> repository [
	^ repository
]

{ #category : 'accessing' }
GoferRepositoryError >> repository: aRepository [
	repository := aRepository
]
