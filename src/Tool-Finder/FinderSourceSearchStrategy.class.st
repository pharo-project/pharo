Class {
	#name : #FinderSourceSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderSourceSearchStrategy class >> strategyName [

	^ #Source
]

{ #category : #testing }
FinderSourceSearchStrategy >> isSourceSearch [

	^ true
]
