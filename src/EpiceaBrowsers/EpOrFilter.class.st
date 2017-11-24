"
My instances decide if an OmEntry is accepted applying an ""or"" logic operation with sub filters.
"
Class {
	#name : #EpOrFilter,
	#superclass : #EpFilter,
	#instVars : [
		'subFilters'
	],
	#category : #'EpiceaBrowsers-Filters'
}

{ #category : #'instance creation' }
EpOrFilter class >> withAll: aCollection [ 
	^ self new 
		subFilters: aCollection;
		yourself
]

{ #category : #comparing }
EpOrFilter >> = other [ 
	^ self class = other class and: [ self subFilters = other subFilters ]
]

{ #category : #filtering }
EpOrFilter >> accepts: anEntry [
	^ self subFilters anySatisfy: [ :aFilter | aFilter accepts: anEntry ]
]

{ #category : #comparing }
EpOrFilter >> hash [
	^ self class hash bitXor: self subFilters hash
]

{ #category : #accessing }
EpOrFilter >> subFilters [
	^ subFilters
]

{ #category : #accessing }
EpOrFilter >> subFilters: aCollectionOfFilters [
	subFilters := aCollectionOfFilters
]
