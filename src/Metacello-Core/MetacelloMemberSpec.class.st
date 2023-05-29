Class {
	#name : #MetacelloMemberSpec,
	#superclass : #MetacelloSpec,
	#instVars : [
		'name',
		'spec'
	],
	#category : #'Metacello-Core-Members'
}

{ #category : #actions }
MetacelloMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [

	self subclassResponsibility
]

{ #category : #actions }
MetacelloMemberSpec >> applyToList: aListSpec [

	self subclassResponsibility
]

{ #category : #printing }
MetacelloMemberSpec >> configMethodCascadeOn: aStream last: lastCascade indent: indent [

	self spec 
		configMethodCascadeOn: aStream 
		member: self 
		last: lastCascade 
		indent: indent
]

{ #category : #printing }
MetacelloMemberSpec >> configMethodOn: aStream indent: indent [

	aStream nextPutAll: self class name asString, ' member: ('.
	self spec configMethodOn: aStream indent: indent.
	aStream nextPutAll: ')'.
]

{ #category : #accessing }
MetacelloMemberSpec >> methodUpdateSelector [

	^self subclassResponsibility
]

{ #category : #accessing }
MetacelloMemberSpec >> name [

	^name
]

{ #category : #accessing }
MetacelloMemberSpec >> name: aString [

	name := aString
]

{ #category : #accessing }
MetacelloMemberSpec >> sourceName [

	^self name
]

{ #category : #accessing }
MetacelloMemberSpec >> spec [

	^spec
]

{ #category : #accessing }
MetacelloMemberSpec >> spec: aMetacelloSpec [

	spec := aMetacelloSpec
]
