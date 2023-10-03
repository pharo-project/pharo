"
I am a Finder search mode when we are looking for classes in the system.
"
Class {
	#name : 'FinderClassesSearchStrategy',
	#superclass : 'FinderSearchStrategy',
	#category : 'Tool-Finder-Base',
	#package : 'Tool-Finder',
	#tag : 'Base'
}

{ #category : 'accessing' }
FinderClassesSearchStrategy class >> strategyName [

	^ #Classes
]

{ #category : 'actions' }
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

{ #category : 'actions' }
FinderClassesSearchStrategy >> constructDictionary [

	| result |
	result := Dictionary new.
	self computeListOfClasses do: [ :class | result at: class put: (class selectors sort: #yourself ascending) ].
	finder resultDictionary: result
]

{ #category : 'accessing' }
FinderClassesSearchStrategy >> findSelectedClassIn: path [

	^ path first item
]

{ #category : 'accessing' }
FinderClassesSearchStrategy >> findSelectedMethodIn: path [

	^ (path at: 2 ifAbsent: [ nil ]) ifNotNil: [ :node | node item ]
]

{ #category : 'testing' }
FinderClassesSearchStrategy >> isClassesSearch [

	^ true
]
