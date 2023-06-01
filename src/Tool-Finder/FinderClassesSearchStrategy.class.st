Class {
	#name : #FinderClassesSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderClassesSearchStrategy class >> strategyName [

	^ #Classes
]

{ #category : #testing }
FinderClassesSearchStrategy >> isClassesSearch [

	^ true
]
