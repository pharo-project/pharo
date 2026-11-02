"
I am a command to switch query browser to flat items list.

By default flat list is sorted by item name (selector in case of methods)
"
Class {
	#name : 'ClySwitchToFlatQueryResultCommand',
	#superclass : 'ClySwitchQueryResultCommand',
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Controlling',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Controlling'
}

{ #category : 'execution' }
ClySwitchToFlatQueryResultCommand >> executeRefactoring [
	self browser switchToFlatResult
]

{ #category : 'testing' }
ClySwitchToFlatQueryResultCommand >> isAppliedToBrowser [
	^self browser showsFlatResult
]

{ #category : 'accessing' }
ClySwitchToFlatQueryResultCommand >> name [
	^'Flat'
]
