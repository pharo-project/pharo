"
I am the version of a project in dev mode: higher than any other version, so nothing replaces me. I keep the description the dev root was created with (github://pharo-graphics/Bloc:master/src), as written: the project is read and loaded from its repository as described, and printed as that description.
"
Class {
	#name : 'MetacelloMVSDevVersion',
	#superclass : 'Object',
	#instVars : [
		'description'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSDevVersion class >> description: aDescriptionString [
	"The dev version of the root described by aDescriptionString, as written"

	^ self new setDescription: aDescriptionString
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> < aVersion [
	"Nothing is higher than a project in dev mode"

	^ false
]

{ #category : 'comparing' }
MetacelloMVSDevVersion >> = anObject [

	^ self class = anObject class and: [ description = anObject description ]
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> baselineOf: aCandidate from: aSource [
	"The Monticello definitions of the baseline of aCandidate, from its repository as my description names it: at the ref it writes"

	^ (MetacelloPlatform current createRepository: (MetacelloRepositorySpec description: description)) baselineOf: aCandidate project
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> description [

	^ description
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> descriptionOf: aCandidate [
	"The description to load aCandidate from: its dev root's, as written"

	^ description
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> fileFormatOf: aCandidate from: aSource [
	"The format of the packages of aCandidate, from its repository as my description names it"

	^ (MetacelloPlatform current createRepository: (MetacelloRepositorySpec description: description)) fileFormat
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> hasMajor: anInteger [
	"A project in dev mode is at a ref, never at a major"

	^ false
]

{ #category : 'comparing' }
MetacelloMVSDevVersion >> hash [

	^ description hash
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> isSameMajorAs: aVersion [
	"A project in dev mode replaces every request of it, so the only version of its repository it meets is itself, selected again"

	^ self = aVersion
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> printOn: aStream [

	aStream nextPutAll: description
]

{ #category : 'accessing' }
MetacelloMVSDevVersion >> setDescription: aDescriptionString [

	description := aDescriptionString
]
