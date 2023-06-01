Class {
	#name : #FinderSelectorsSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderSelectorsSearchStrategy class >> strategyName [

	^ #Selectors
]

{ #category : #testing }
FinderSelectorsSearchStrategy >> isSelectorsSearch [

	^ true
]
