"
I am a version of a project as its repository publishes it: a name (like v1.2.0) and the date it was made, like a git tag. Repositories answer their versions with MCRepository>>versions; I stand for those versions where no repository is at hand (recordings, tests).

This is not a MetacelloVersion, a version from a configuration.
"
Class {
	#name : 'MetacelloProjectVersion',
	#superclass : 'Object',
	#instVars : [
		'name',
		'date'
	],
	#category : 'Metacello-Core',
	#package : 'Metacello-Core'
}

{ #category : 'instance creation' }
MetacelloProjectVersion class >> name: aString [
	"A version whose date is not known"

	^ self name: aString date: nil
]

{ #category : 'instance creation' }
MetacelloProjectVersion class >> name: aString date: aDateAndTime [

	^ self new setName: aString date: aDateAndTime
]

{ #category : 'accessing' }
MetacelloProjectVersion >> date [
	"When I was made, or nil when it is not known"

	^ date
]

{ #category : 'accessing' }
MetacelloProjectVersion >> name [

	^ name
]

{ #category : 'accessing' }
MetacelloProjectVersion >> printOn: aStream [

	aStream nextPutAll: name
]

{ #category : 'accessing' }
MetacelloProjectVersion >> setName: aString date: aDateAndTime [

	name := aString.
	date := aDateAndTime
]
