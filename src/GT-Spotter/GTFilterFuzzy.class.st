Class {
	#name : #GTFilterFuzzy,
	#superclass : #GTOrderedFilter,
	#category : #'GT-Spotter-Filters'
}

{ #category : #private }
GTFilterFuzzy >> applyFilterWithQuery [
	
	| result |

	result := FuzzyMatcher allSortedByScoreMatching: query 
		in: self allItems
		by: [ :each | self itemFilterNameFor: each ].
	
	self 
		addItems: (result takeFirst: self itemsLimit);
		filteredItems: result

]
