Class {
	#name : #FinderExamplesSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderExamplesSearchStrategy class >> strategyName [

	^ #Examples
]

{ #category : #testing }
FinderExamplesSearchStrategy >> isExamplesSearch [

	^ true
]
