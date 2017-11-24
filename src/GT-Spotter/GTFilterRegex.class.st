Class {
	#name : #GTFilterRegex,
	#superclass : #GTUnorderedFilter,
	#instVars : [
		'regex'
	],
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTFilterRegex >> applyFilterInContext: aSpotterContext error: exception [
	super applyFilterInContext: aSpotterContext error: exception.
	^ (GTFilterSubstrings new
		caseSensitive: self caseSensitive;
		yourself) value: aSpotterContext
]

{ #category : #private }
GTFilterRegex >> isMatchedItem: anItem [
	^ self regex matches: (self itemFilterNameFor: anItem)
]

{ #category : #private }
GTFilterRegex >> prepareFilterInContext: aSpotterContext [
	super prepareFilterInContext: aSpotterContext.
	
	self regex: (caseSensitive " performance optimization "
		ifTrue: [ self query asRegex ] 
		ifFalse: [ self query asRegexIgnoringCase ])
]

{ #category : #accessing }
GTFilterRegex >> regex [
	^ regex
]

{ #category : #accessing }
GTFilterRegex >> regex: anObject [
	regex := anObject
]
