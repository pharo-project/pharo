Class {
	#name : #GTUnorderedFilter,
	#superclass : #GTStringFilter,
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTUnorderedFilter >> doApplyFilterWithQuery [

	| unorderedMatchedItems |

	unorderedMatchedItems := OrderedCollection new.
	self allItemsDo: [ :each | 
		(self isMatchedItem: each)
				ifTrue: [
					unorderedMatchedItems add: each.
					self addItem: each.
					unorderedMatchedItems size = self itemsLimit ifTrue: [ GTLimitArrived signal ] ] ].
	self filteredItems: unorderedMatchedItems
]

{ #category : #private }
GTUnorderedFilter >> isMatchedItem: anItem [
	^ false
]
