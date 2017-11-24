"
I represent the null OmStore (Null Object design pattern).
"
Class {
	#name : #OmNullStore,
	#superclass : #OmStore,
	#category : #'Ombu-Stores'
}

{ #category : #enumerating }
OmNullStore >> entriesDo: aBlockClosure [
	"Evaluate aBlockClosure with every entries this store contains"

]

{ #category : #accessing }
OmNullStore >> entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [
	"Look for an entry, evaluating either the first block closure if present or the second if absent."

	^ absentBlockClosure value
]

{ #category : #accessing }
OmNullStore >> headReference [
	"Answer a reference to the last entry added to this store."

	^ OmNullReference uniqueInstance
]

{ #category : #writing }
OmNullStore >> newEntry: anEntry [

	self shouldNotImplement 
]

{ #category : #refreshing }
OmNullStore >> refresh [

]

{ #category : #accessing }
OmNullStore >> writingFileReference [

	^ self shouldNotImplement
]
