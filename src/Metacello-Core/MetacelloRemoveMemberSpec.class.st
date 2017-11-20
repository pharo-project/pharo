Class {
	#name : #MetacelloRemoveMemberSpec,
	#superclass : #MetacelloMemberSpec,
	#category : #Metacello-Core-Members
}

{ #category : #actions }
MetacelloRemoveMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [

	removeBlock value: self
]

{ #category : #actions }
MetacelloRemoveMemberSpec >> applyToList: aListSpec [

	aListSpec remove: self
]

{ #category : #accessing }
MetacelloRemoveMemberSpec >> methodUpdateSelector [

	^#remove:
]
