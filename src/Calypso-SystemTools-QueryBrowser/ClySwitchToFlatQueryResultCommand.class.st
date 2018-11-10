"
I am a command to switch query browser to flat items list.

By default flat list is sorted by item name (selector in case of methods)
"
Class {
	#name : #ClySwitchToFlatQueryResultCommand,
	#superclass : #ClySwitchQueryResultCommand,
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Controlling'
}

{ #category : #execution }
ClySwitchToFlatQueryResultCommand >> requiredQueryResult [
	^ClySortedQueryResult using: ClySortSystemItemFunction ascending
]
