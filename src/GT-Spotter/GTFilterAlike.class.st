Class {
	#name : #GTFilterAlike,
	#superclass : #GTOrderedFilter,
	#instVars : [
		'lowerThreshold',
		'upperThreshold'
	],
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTFilterAlike >> applyFilterWithQuery [
	| allItemsPrepocessed allItemsPrepocessedAndSorted alikeMatches otherMatches upper lower |
	alikeMatches := OrderedCollection  new.
	otherMatches := OrderedCollection new.	
	" calculate the <similarity> of all items and throw away everything that is below the lower threshold (irrelevant) "			
	allItemsPrepocessed := OrderedCollection streamContents: [ :allItems | 
		lower := lowerThreshold * query size.
		self allItemsDo: [ :each | 
			| weight |
			(weight :=  (self itemFilterNameFor: each) alike: query) > lower ifTrue: [
				allItems nextPut: (Array with: weight with: each) ] ] ].
	" sort all items using <similarity> "
	allItemsPrepocessedAndSorted := allItemsPrepocessed asSortedCollection: [ :a :b | a first > b first ].
	" calculate the upper threshold "
	upper := allItemsPrepocessed isEmpty 
		ifTrue: [ upperThreshold ]
		ifFalse: [ (allItemsPrepocessedAndSorted collect: [ :each | each first ]) median " try an adaptive threshold (slow if not presorted) " ].
	" stream all items based on upper and lower threshold "
	allItemsPrepocessedAndSorted do: [ :each | 
		each first >= upper 
			ifTrue: [
				alikeMatches add: each last.
				alikeMatches size > self itemsLimit ifFalse: [ 
					self addItem: each ] ]
			ifFalse:[ otherMatches add: each last ] ].
	" push less relevant matches onto the result "
	(alikeMatches size < self itemsLimit) ifTrue: [ 
		(otherMatches first: ((self itemsLimit - alikeMatches size) min: otherMatches size)) do: [ :each |
			self addItem: each ] ].
	self filteredItems: alikeMatches , otherMatches 
]

{ #category : #'accessing-defaults' }
GTFilterAlike >> defaultLowerThreshold [
	" lower threshold: to cut off the large irrelevent chunk before sorting/filtering is appplied "
	^ 0.33
]

{ #category : #'accessing-defaults' }
GTFilterAlike >> defaultUpperThreshold [
	" upper threshold: to keep a reasonably sized set of interesting matches "
	^ 0.45
]

{ #category : #accessing }
GTFilterAlike >> lowerThreshold [
	^ lowerThreshold
]

{ #category : #accessing }
GTFilterAlike >> lowerThreshold: anObject [
	lowerThreshold := anObject
]

{ #category : #private }
GTFilterAlike >> prepareFilterInContext: aSpotterContext [
	super prepareFilterInContext: aSpotterContext.
	
	lowerThreshold ifNil: [ lowerThreshold := self defaultLowerThreshold ]. " performance optimization "
	upperThreshold ifNil: [ upperThreshold := self defaultUpperThreshold ]. " performance optimization "
]

{ #category : #accessing }
GTFilterAlike >> upperThreshold [
	^ upperThreshold
]

{ #category : #accessing }
GTFilterAlike >> upperThreshold: anObject [
	upperThreshold := anObject
]
