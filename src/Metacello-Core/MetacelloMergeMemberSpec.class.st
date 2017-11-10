Class {
	#name : #MetacelloMergeMemberSpec,
	#superclass : #MetacelloMemberSpec,
	#category : #Metacello-Core-Members
}

{ #category : #actions }
MetacelloMergeMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [

	mergeBlock value: self
]

{ #category : #actions }
MetacelloMergeMemberSpec >> applyToList: aListSpec [

	aListSpec merge: self
]

{ #category : #accessing }
MetacelloMergeMemberSpec >> methodUpdateSelector [

	^#with:
]
