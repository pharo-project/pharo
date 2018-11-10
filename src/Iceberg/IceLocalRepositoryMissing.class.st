"
The repository code directory does not exist, maybe because the image was moved. 

Default action is to clone the repository again, but signal an error and allow for other components to react accordingly.
"
Class {
	#name : #IceLocalRepositoryMissing,
	#superclass : #IceError,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #exceptioninstantiator }
IceLocalRepositoryMissing class >> signalFor: repository [
	^ self new 
		repository: repository;
		signal
]

{ #category : #accessing }
IceLocalRepositoryMissing >> repository [
	^ repository
]

{ #category : #accessing }
IceLocalRepositoryMissing >> repository: anObject [
	repository := anObject
]
