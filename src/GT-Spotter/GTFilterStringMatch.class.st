Class {
	#name : #GTFilterStringMatch,
	#superclass : #GTUnorderedFilter,
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTFilterStringMatch >> isMatchedItem: anItem [
	^ query match: (self itemFilterNameFor: anItem)
]
