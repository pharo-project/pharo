Class {
	#name : #FinderClassesSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderClassesSearchStrategy class >> strategyName [

	^ #Classes
]

{ #category : #actions }
FinderClassesSearchStrategy >> computeListOfClassesFor: aFinder [

	| regex result |
	regex := aFinder searchingString asRegex.

	result := OrderedCollection new.

	aFinder packagesSelection classesAndTraitsDo: [ :class |
		(aFinder useRegEx
			 ifTrue: [ regex search: class name ]
			 ifFalse: [ class name includesSubstring: aFinder searchingString caseSensitive: false ]) ifTrue: [ result add: class ] ].

	^ result
]

{ #category : #actions }
FinderClassesSearchStrategy >> constructDictionaryOf: aFinder [

	| result |
	result := Dictionary new.
	(self computeListOfClassesFor: aFinder) do: [ :class | result at: class put: (class selectors sort: #yourself ascending) ].
	aFinder resultDictionary: result
]

{ #category : #testing }
FinderClassesSearchStrategy >> isClassesSearch [

	^ true
]
