"
I browse the result selected by user of DrTests.
"
Class {
	#name : #DTResultBrowseCommand,
	#superclass : #DTLeafResultCommand,
	#category : #'DrTests-Commands'
}

{ #category : #hooks }
DTResultBrowseCommand >> execute [
	self context browseSelectedResult
]

{ #category : #hooks }
DTResultBrowseCommand >> initialize [
	super initialize.
	self name: 'Browse'
]
