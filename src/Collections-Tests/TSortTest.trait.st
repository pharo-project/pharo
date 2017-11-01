"
A trait for test purposes
"
Trait {
	#name : #TSortTest,
	#category : 'Collections-Tests-Abstract'
}

{ #category : #requirements }
TSortTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"
	^ self explicitRequirement
	
]

{ #category : #'tests - fixture' }
TSortTest >> test0SortingArrayedTest [
	| tmp sorted |
	" an unsorted collection of number "
	self unsortedCollection.
	self unsortedCollection do: [ :each | each isNumber ].
	sorted := true.
	self unsortedCollection
		pairsDo: [ :each1 :each2 | 
			each2 < each1
				ifTrue: [ sorted := false ] ].
	self assert: sorted = false.	" a collection of number sorted in an ascending order"
	self sortedInAscendingOrderCollection.
	self sortedInAscendingOrderCollection do: [ :each | each isNumber ].
	tmp := self sortedInAscendingOrderCollection at: 1.
	self sortedInAscendingOrderCollection
		do: [ :each | 
			self assert: each >= tmp.
			tmp := each ]
]

{ #category : #'tests - sorting' }
TSortTest >> testIsSorted [
	self assert: self sortedInAscendingOrderCollection isSorted.
	self deny: self unsortedCollection isSorted
]

{ #category : #'tests - sorting' }
TSortTest >> testIsSortedBy [
	self assert: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a<b]).
	self deny: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a>b]).

]

{ #category : #'tests - sorting' }
TSortTest >> testSort [
	| result tmp |
	result := self unsortedCollection sort.
	tmp := result at: 1.
	result do:
		[:each | self assert: each>=tmp. tmp:= each. ].
]

{ #category : #'tests - sorting' }
TSortTest >> testSortUsingSortBlock [
	| result tmp |
	result := self unsortedCollection sort: [:a :b | a>b].
	tmp := result at: 1.
	result do:
		[:each | self assert: each<=tmp. tmp:= each. ].
]

{ #category : #tests }
TSortTest >> testSorted [
	| result tmp unsorted |
	unsorted := self unsortedCollection.
	result := unsorted sorted.
	self deny: unsorted == result.
	tmp := result at: 1.
	result
		do: [ :each | 
			self assert: each >= tmp.
			tmp := each ]
]

{ #category : #'tests - sorting' }
TSortTest >> testSortedUsingBlock [
	| result tmp |
	result := self unsortedCollection sorted:  [:a :b | a>b]..
	tmp := result at: 1.
	result do:
		[:each | self assert: each<=tmp. tmp:= each. ].
]

{ #category : #requirements }
TSortTest >> unsortedCollection [
"Return a collection that is not yet sorted"
	^self explicitRequirement
]
