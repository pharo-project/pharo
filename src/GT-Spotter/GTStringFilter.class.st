"
I provide the functionality for filtering items based on a query.

I return the list of all items that match the filter but my subclasses should only send to the processor the first  #itemsLimit items .

"
Class {
	#name : #GTStringFilter,
	#superclass : #GTFilter,
	#instVars : [
		'caseSensitive',
		'query'
	],
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTStringFilter >> applyFilter [
	self hasQuery ifFalse: [ 
		self applyFilterWithoutQuery.
		^ self allItems. " return the unfiltered items (all) - used for divein (no filtering appied) "  ].
	self applyFilterWithQuery.
	^ self filteredItems " return the filtered items only "
]

{ #category : #private }
GTStringFilter >> applyFilterWithQuery [
	
]

{ #category : #private }
GTStringFilter >> applyFilterWithoutQuery [
	" WARNING: this is not a copy of GTNullFilter>>#applyFilterWithoutQuery !!! "
	self streamed 
		ifTrue: [
			" this will be more responsive (faster) for very large collections and/or expensive filters "
			| reducedItems |
			self filteredItems: (reducedItems := OrderedCollection new).
			self allItems withIndexDo: [ :each :index |
				index <= self itemsLimit ifTrue: [ 
					reducedItems add: each.
					self addItem: each ] ] ]
		ifFalse: [
			" this will be much much faster for small collections and/or very quick filters " 
			self filteredItems: (self allItems first: (self itemsLimit min: self allItems size)).
			self addItems: self filteredItems ]
]

{ #category : #accessing }
GTStringFilter >> caseSensitive [
	^ caseSensitive
]

{ #category : #accessing }
GTStringFilter >> caseSensitive: anObject [
	caseSensitive := anObject
]

{ #category : #'accessing-defaults' }
GTStringFilter >> defaultCaseSensitive [
	^ false
]

{ #category : #testing }
GTStringFilter >> hasQuery [
	^ self query isEmptyOrNil not
]

{ #category : #private }
GTStringFilter >> prepareFilterInContext: aSpotterContext [
	super prepareFilterInContext: aSpotterContext.
	
	caseSensitive ifNil: [ caseSensitive := self defaultCaseSensitive ]. " performance optimization "
	self query: (caseSensitive " performance optimization "
		ifTrue: [ self context textTrimmed ]
		ifFalse: [ self context textLowercase ])
]

{ #category : #accessing }
GTStringFilter >> query [
	^ query
]

{ #category : #accessing }
GTStringFilter >> query: anObject [
	query := anObject
]
