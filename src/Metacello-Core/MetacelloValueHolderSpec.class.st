Class {
	#name : #MetacelloValueHolderSpec,
	#superclass : #MetacelloSpec,
	#instVars : [
		'value'
	],
	#category : #'Metacello-Core-Specs'
}

{ #category : #printing }
MetacelloValueHolderSpec >> configMethodOn: aStream indent: indent [

	aStream tab: indent; nextPutAll: 'spec value: ', self value printString
]

{ #category : #merging }
MetacelloValueHolderSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #value put: value.
	^map
]

{ #category : #private }
MetacelloValueHolderSpec >> setAuthorInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setAuthor: self
]

{ #category : #private }
MetacelloValueHolderSpec >> setBlessingInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setBlessing: self
]

{ #category : #private }
MetacelloValueHolderSpec >> setDescriptionInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setDescription: self
]

{ #category : #private }
MetacelloValueHolderSpec >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPostLoadDoIt: self
]

{ #category : #private }
MetacelloValueHolderSpec >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPreLoadDoIt: self
]

{ #category : #private }
MetacelloValueHolderSpec >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: self
]

{ #category : #querying }
MetacelloValueHolderSpec >> value [
	^ value
]

{ #category : #accessing }
MetacelloValueHolderSpec >> value: anObject [
	value := anObject
]

{ #category : #construction }
MetacelloValueHolderSpec >> value: anObject constructor: aVersionConstructor [
    aVersionConstructor valueForValueHolder: anObject
]
