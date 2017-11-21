"
A TrAndFilter is a chain of AND filters.
"
Class {
	#name : #EpAndFilter,
	#superclass : #EpFilter,
	#instVars : [
		'subFilters'
	],
	#category : #EpiceaBrowsers-Filters
}

{ #category : #'instance creation' }
EpAndFilter class >> withAll: aCollection [ 
	^ self new 
		subFilters: aCollection;
		yourself
]

{ #category : #comparing }
EpAndFilter >> = other [ 
	^ self class = other class and: [ self subFilters = other subFilters ]
]

{ #category : #filtering }
EpAndFilter >> accepts: anEntry [
	^ self subFilters allSatisfy: [ :aFilter | aFilter accepts: anEntry ]
]

{ #category : #comparing }
EpAndFilter >> hash [
	^ self class hash bitXor: self subFilters hash
]

{ #category : #accessing }
EpAndFilter >> subFilters [
	^ subFilters
]

{ #category : #accessing }
EpAndFilter >> subFilters: aCollectionOfFilters [
	subFilters := aCollectionOfFilters
]
