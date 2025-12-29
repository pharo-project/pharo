Class {
	#name : 'MetacelloVersionDoesNotExistError',
	#superclass : 'MetacelloError',
	#instVars : [
		'project'
	],
	#category : 'Metacello-Core-Exceptions',
	#package : 'Metacello-Core',
	#tag : 'Exceptions'
}

{ #category : 'instance creation' }
MetacelloVersionDoesNotExistError class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'accessing' }
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

{ #category : 'signaling' }
MetacelloVersionDoesNotExistError >> signal [
	"GemStone needs this to print exception correctly"

	self messageText: self description.
	^ super signal
]
