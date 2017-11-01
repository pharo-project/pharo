Class {
	#name : #MetacelloCopyMemberSpec,
	#superclass : #MetacelloMemberSpec,
	#instVars : [
		'sourceName'
	],
	#category : 'Metacello-Core-Members'
}

{ #category : #actions }
MetacelloCopyMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [
    copyBlock value: self
]

{ #category : #actions }
MetacelloCopyMemberSpec >> applyToList: aListSpec [

	aListSpec copy: self
]

{ #category : #accessing }
MetacelloCopyMemberSpec >> methodUpdateSelector [

	^#copy:
]

{ #category : #accessing }
MetacelloCopyMemberSpec >> sourceName [

	^sourceName
]

{ #category : #accessing }
MetacelloCopyMemberSpec >> sourceName: aString [

	sourceName := aString
]
