Class {
	#name : 'MetacelloVersionDoesNotExistError',
	#superclass : 'MetacelloError',
	#instVars : [
		'project'
	],
	#category : 'Metacello-Core-Exceptions-Errors',
	#package : 'Metacello-Core',
	#tag : 'Exceptions-Errors'
}

{ #category : 'instance creation' }
MetacelloVersionDoesNotExistError class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'printing' }
MetacelloVersionDoesNotExistError >> description [

	^ 'Not version defined in ' , self project label
]

{ #category : 'accessing' }
MetacelloVersionDoesNotExistError >> project [
	^ project
]

{ #category : 'accessing' }
MetacelloVersionDoesNotExistError >> project: anObject [
	project := anObject
]

{ #category : 'Signaling' }
MetacelloVersionDoesNotExistError >> signal [
  "GemStone needs this to print exception correctly"

  self messageText: self description.
  ^ super signal
]
