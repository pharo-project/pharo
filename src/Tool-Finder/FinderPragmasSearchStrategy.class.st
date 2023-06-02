Class {
	#name : #FinderPragmasSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderPragmasSearchStrategy class >> strategyName [

	^ #Pragmas
]

{ #category : #actions }
FinderPragmasSearchStrategy >> constructDictionary [

	| dictionary |
	dictionary := self pragmaSearch.
	dictionary keysDo: [ :k |
		| result |
		result := Dictionary new.
		(dictionary at: k) do: [ :method |
			| key value |
			key := method selector.
			value := method methodClass.
			(result at: key ifAbsentPut: OrderedCollection new) add: value ].
		dictionary at: k put: result ].
	finder resultDictionary: dictionary
]

{ #category : #testing }
FinderPragmasSearchStrategy >> isPragmasSearch [

	^ true
]

{ #category : #actions }
FinderPragmasSearchStrategy >> pragmaSearch [

	| result byCondition |
	"I choose a dictionary here because the next step is to group result by pragmas."
	result := Dictionary new.

	byCondition := finder useRegEx
		               ifTrue: [ [ :pragma | pragma selector matchesRegexIgnoringCase: finder searchingString ] ]
		               ifFalse: [ [ :pragma | pragma selector includesSubstring: finder searchingString caseSensitive: false ] ].

	(Pragma all select: byCondition) do: [ :pragma | (result at: pragma selector ifAbsentPut: OrderedCollection new) add: pragma method ].
	^ result
]
