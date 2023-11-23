Class {
	#name : 'MetacelloValueHolderSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'value'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'private' }
MetacelloValueHolderSpec >> asMetacelloValueHolderFor: aMetacelloVersionSpec [

	^ self
]

{ #category : 'printing' }
MetacelloValueHolderSpec >> configMethodOn: aStream indent: indent [

	aStream tab: indent; nextPutAll: 'spec value: ', self value printString
]

{ #category : 'merging' }
MetacelloValueHolderSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #value put: value.
	^map
]

{ #category : 'private' }
MetacelloValueHolderSpec >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPostLoadDoIt: self
]

{ #category : 'private' }
MetacelloValueHolderSpec >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPreLoadDoIt: self
]

{ #category : 'querying' }
MetacelloValueHolderSpec >> value [
	^ value
]

{ #category : 'accessing' }
MetacelloValueHolderSpec >> value: anObject [
	value := anObject
]
