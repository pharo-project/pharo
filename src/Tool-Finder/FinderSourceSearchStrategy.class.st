"
I am a Finder search mode used to look for a string inside the whole Pharo image source code.
"
Class {
	#name : 'FinderSourceSearchStrategy',
	#superclass : 'FinderSearchStrategy',
	#category : 'Tool-Finder-Base',
	#package : 'Tool-Finder',
	#tag : 'Base'
}

{ #category : 'accessing' }
FinderSourceSearchStrategy class >> strategyName [

	^ #Source
]

{ #category : 'actions' }
FinderSourceSearchStrategy >> constructDictionary [

	| result |
	result := Dictionary new.
	(finder useRegEx
		 ifTrue: [ self sourceRegexSearch ]
		 ifFalse: [ self sourceStringSearch ]) do: [ :method |
		| key value |
		key := method selector.
		value := method methodClass.
		(result includesKey: key)
			ifTrue: [ (result at: key) add: value ]
			ifFalse: [
				result at: key put: (OrderedCollection new
						 add: value;
						 yourself) ] ].
	finder resultDictionary: result
]

{ #category : 'accessing' }
FinderSourceSearchStrategy >> findSelectedClassIn: path [

	^ (path at: 2 ifAbsent: [ nil ]) ifNotNil: [ :node | node item ]
]

{ #category : 'accessing' }
FinderSourceSearchStrategy >> findSelectedMethodIn: path [

	^ path first item
]

{ #category : 'actions' }
FinderSourceSearchStrategy >> sourceRegexSearch [

	| regex |
	regex := finder searchingString asRegex.
	^ self methodSearch: [ :method | regex search: method sourceCode ]
]

{ #category : 'actions' }
FinderSourceSearchStrategy >> sourceStringSearch [

	^ self methodSearch: [ :method | method sourceCode includesSubstring: finder searchingString caseSensitive: false ]
]
