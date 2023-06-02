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
FinderClassesSearchStrategy >> computeListOfClasses [

	| regex result |
	regex := finder searchingString asRegex.

	result := OrderedCollection new.

	finder packagesSelection classesAndTraitsDo: [ :class |
		(finder useRegEx
			 ifTrue: [ regex search: class name ]
			 ifFalse: [ class name includesSubstring: finder searchingString caseSensitive: false ]) ifTrue: [ result add: class ] ].

	^ result
]

{ #category : #actions }
FinderClassesSearchStrategy >> constructDictionary [

	| result |
	result := Dictionary new.
	self computeListOfClasses do: [ :class | result at: class put: (class selectors sort: #yourself ascending) ].
	finder resultDictionary: result
]

{ #category : #testing }
FinderClassesSearchStrategy >> isClassesSearch [

	^ true
]
