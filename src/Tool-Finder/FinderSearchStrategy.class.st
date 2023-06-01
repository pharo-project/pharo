Class {
	#name : #FinderSearchStrategy,
	#superclass : #Object,
	#category : #'Tool-Finder-Base'
}

{ #category : #testing }
FinderSearchStrategy class >> isAbstract [

	^ self = FinderSearchStrategy
]

{ #category : #accessing }
FinderSearchStrategy class >> strategyName [

	^ self subclassResponsibility
]

{ #category : #accessing }
FinderSearchStrategy class >> strategyNamed: aSymbol [

	^ self subclasses detect: [ :class | class strategyName = aSymbol ]
]

{ #category : #testing }
FinderSearchStrategy >> isClassesSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isExamplesSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isPragmasSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isSelectorsSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isSourceSearch [

	^ false
]
