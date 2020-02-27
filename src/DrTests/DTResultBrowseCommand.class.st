"
I browse the result selected by user of DrTests.
"
Class {
	#name : #DTResultBrowseCommand,
	#superclass : #DTLeafResultCommand,
	#category : #'DrTests-Commands'
}

{ #category : #testing }
DTResultBrowseCommand >> canBeExecuted [
	^ self resultSelected canBeBrowsed
]

{ #category : #hooks }
DTResultBrowseCommand >> execute [
	self drTest browseSelectedResult
]

{ #category : #hooks }
DTResultBrowseCommand >> initialize [
	super initialize.
	self name: 'Browse'
]
