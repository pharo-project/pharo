Class {
	#name : #GTFilterSubstrings,
	#superclass : #GTUnorderedFilter,
	#instVars : [
		'separators'
	],
	#category : #'GT-Spotter-Filters'
}

{ #category : #'accessing-defaults' }
GTFilterSubstrings >> defaultSeparators [
	^ ' '
]

{ #category : #private }
GTFilterSubstrings >> isMatchedItem: anItem [
	| itemName |
	itemName := self itemFilterNameFor: anItem.
	^ query allSatisfy: [ :fragment | 
		itemName includesSubstring: fragment caseSensitive: self caseSensitive ]
]

{ #category : #private }
GTFilterSubstrings >> prepareFilterInContext: aSpotterContext [
	super prepareFilterInContext: aSpotterContext.
	
	separators ifNil: [  separators := self defaultSeparators  ]. " performance optimization "
	self query: (self query substrings: self separators) asSet asArray
]

{ #category : #accessing }
GTFilterSubstrings >> separators [
	^ separators
]

{ #category : #accessing }
GTFilterSubstrings >> separators: anObject [
	separators := anObject
]
