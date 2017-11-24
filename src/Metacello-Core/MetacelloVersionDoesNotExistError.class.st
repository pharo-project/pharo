Class {
	#name : #MetacelloVersionDoesNotExistError,
	#superclass : #Error,
	#instVars : [
		'project',
		'versionString'
	],
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #'instance creation' }
MetacelloVersionDoesNotExistError class >> project: aMetacelloProject versionString: aVersionString [

	^(self new)
		project: aMetacelloProject;
		versionString: aVersionString;
		yourself
]

{ #category : #printing }
MetacelloVersionDoesNotExistError >> description [
    "Return a textual description of the exception."

    ^ 'Version ' , self versionString printString , ' is not defined in ' , self project label
        , '. Possible versions include: ' , self possibleVersions printString
]

{ #category : #accessing }
MetacelloVersionDoesNotExistError >> possibleVersions [
    ^ self project symbolicVersionSymbols , (self project versions collect: [ :each | each versionString ])
]

{ #category : #accessing }
MetacelloVersionDoesNotExistError >> project [
	^ project
]

{ #category : #accessing }
MetacelloVersionDoesNotExistError >> project: anObject [
	project := anObject
]

{ #category : #Signaling }
MetacelloVersionDoesNotExistError >> signal [
  "GemStone needs this to print exception correctly"

  self messageText: self description.
  ^ super signal
]

{ #category : #accessing }
MetacelloVersionDoesNotExistError >> versionString [
	^ versionString
]

{ #category : #accessing }
MetacelloVersionDoesNotExistError >> versionString: anObject [
	versionString := anObject
]
