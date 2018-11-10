"
I'm a generic command  for iceberg repositories.
"
Class {
	#name : #IceRepositoryCommand,
	#superclass : #Object,
	#instVars : [
		'repository',
		'success'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceRepositoryCommand >> beSuccess [
	success := true
]

{ #category : #execute }
IceRepositoryCommand >> execute [ 
	self subclassResponsibility
]

{ #category : #initialization }
IceRepositoryCommand >> initialize [
	super initialize.
	success := false
]

{ #category : #testing }
IceRepositoryCommand >> isSuccess [
	^ success
]

{ #category : #accessing }
IceRepositoryCommand >> repository [
	^ repository
]

{ #category : #accessing }
IceRepositoryCommand >> repository: anObject [
	repository := anObject
]
