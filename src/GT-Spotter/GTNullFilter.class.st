Class {
	#name : #GTNullFilter,
	#superclass : #GTStringFilter,
	#category : #'GT-Spotter-Filters'
}

{ #category : #public }
GTNullFilter class >> gtListFilter [
	^ GTFilterSubstring new
]

{ #category : #private }
GTNullFilter >> applyFilterWithQuery [
	self streamed 
		ifTrue: [
			" this will be more responsive (faster) for very large collections and/or expensive filters "
			| allItems |
			self filteredItems: (allItems := OrderedCollection new).
			self allItems withIndexDo: [ :each :index |
				allItems add: each.
				index <= self itemsLimit ifTrue: [ 
					self addItem: each ] ] ]
		ifFalse: [
			" this will be much much faster for small collections and/or very quick filters " 
			self filteredItems: self allItems.
			self addItems: (self allItems first: (self itemsLimit min: self allItems size)) ]
]

{ #category : #public }
GTNullFilter >> gtListFilter [
	^ GTFilterSubstring new
]
