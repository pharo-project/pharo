"
A filter for blocks
"
Class {
	#name : #BlockFilter,
	#superclass : #StackFilter,
	#instVars : [
		'block'
	],
	#category : #'Debugger-Filters'
}

{ #category : #'instance creation' }
BlockFilter class >> forBlock: aBlockClosure [
	^ self new block: aBlockClosure
]

{ #category : #accessing }
BlockFilter >> block [
	 ^block
]

{ #category : #accessing }
BlockFilter >> block: aBlockClosure [
	block:= aBlockClosure
]

{ #category : #testing }
BlockFilter >> shouldDisplay: aContext [
	 ^self block value: aContext
]
