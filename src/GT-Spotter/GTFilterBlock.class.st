Class {
	#name : #GTFilterBlock,
	#superclass : #GTUnorderedFilter,
	#instVars : [
		'matcher'
	],
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTFilterBlock >> isMatchedItem: anItem [
	^ self matcher 
		ifNotNil: [ self matcher value: anItem value: self context ]
		ifNil: [ (self itemFilterNameFor: anItem) includesSubstring: query caseSensitive: caseSensitive ]
		
]

{ #category : #accessing }
GTFilterBlock >> matcher [
	^ matcher
]

{ #category : #accessing }
GTFilterBlock >> matcher: anObject [
	matcher := anObject
]
