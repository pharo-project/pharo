Class {
	#name : 'MetacelloAddMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Members',
	#package : 'Metacello-Core',
	#tag : 'Members'
}

{ #category : 'actions' }
MetacelloAddMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [

	addBlock value: self
]

{ #category : 'actions' }
MetacelloAddMemberSpec >> applyToList: aListSpec [

	aListSpec add: self
]

{ #category : 'accessing' }
MetacelloAddMemberSpec >> methodUpdateSelector [

	^#overrides:
]
