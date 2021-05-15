"
I browse the result selected by user of DrTests.
"
Class {
	#name : #DTResultBrowseCommand,
	#superclass : #DTLeafResultCommand,
	#category : #'DrTests-Commands'
}

{ #category : #defaults }
DTResultBrowseCommand class >> defaultName [

	^ 'Browse'
]

{ #category : #testing }
DTResultBrowseCommand >> canBeExecuted [
	^ self resultSelected canBeBrowsed
]

{ #category : #hooks }
DTResultBrowseCommand >> execute [
	self context browseSelectedResult
]
