Class {
	#name : 'MetacelloMemberSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'spec'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'testing' }
MetacelloMemberSpec class >> isAbstract [

	^ self = MetacelloMemberSpec
]

{ #category : 'instance creation' }
MetacelloMemberSpec class >> spec: aSpec [

	^ self new
		  spec: aSpec;
		  yourself
]

{ #category : 'actions' }
MetacelloMemberSpec >> applyTo: aCollection [

	self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloMemberSpec >> name [

	^ self spec name
]

{ #category : 'printing' }
MetacelloMemberSpec >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' [';
		nextPutAll: self name;
		nextPutAll: ']'
]

{ #category : 'accessing' }
MetacelloMemberSpec >> spec [

	^spec
]

{ #category : 'accessing' }
MetacelloMemberSpec >> spec: aMetacelloSpec [

	spec := aMetacelloSpec
]
