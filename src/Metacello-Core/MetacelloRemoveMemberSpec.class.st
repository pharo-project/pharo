Class {
	#name : 'MetacelloRemoveMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#instVars : [
		'name'
	],
	#category : 'Metacello-Core-Members',
	#package : 'Metacello-Core',
	#tag : 'Members'
}

{ #category : 'actions' }
MetacelloRemoveMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [

	removeBlock value: self
]

{ #category : 'actions' }
MetacelloRemoveMemberSpec >> applyToList: aListSpec [

	aListSpec remove: self
]

{ #category : 'accessing' }
MetacelloRemoveMemberSpec >> methodUpdateSelector [

	^#remove:
]

{ #category : 'accessing' }
MetacelloRemoveMemberSpec >> name [

	^ name ifNil: [ super name ]
]

{ #category : 'accessing' }
MetacelloRemoveMemberSpec >> name: aString [

	name := aString
]
