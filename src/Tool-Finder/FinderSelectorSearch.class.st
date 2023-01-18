"
I implement a search for selectors in a given environment.

I am a subclass of ̀FinderSearch̀ and am used by ̀FinderModel̀ to perform searches
for selectors.

"
Class {
	#name : #FinderSelectorSearch,
	#superclass : #FinderSearch,
	#category : #'Tool-Finder-Search'
}

{ #category : #private }
FinderSelectorSearch >> buildResult: aListOfMethods [

	| result |
	result := Dictionary new.
	aListOfMethods do: [ :method |
		| key value |
		key := method selector.
		value := method methodClass.
		(result includesKey: key)
			ifTrue: [ (result at: key) add: value ]
			ifFalse: [
				result at: key put: (OrderedCollection new
						 add: value;
						 yourself) ] ].
	^ result
]

{ #category : #information }
FinderSelectorSearch >> name [
	"Returns the name of the search."

	^ 'Selectors'
]

{ #category : #searching }
FinderSelectorSearch >> searchByRegex: aRegex in: anEnvironment [
	"Perform a search given aRegex in anEnvironment."

	^ self buildResult: (self searchMethods: [ :method |
			   aRegex search: method selector asString ] in: anEnvironment)
]

{ #category : #searching }
FinderSelectorSearch >> searchByString: aString in: anEnvironment [
	"Perform a search given aString in anEnvironment."

	^ self buildResult: (self searchMethods: [ :method |
			   method selector includesSubstring: aString caseSensitive: false ] in: anEnvironment)
]

{ #category : #private }
FinderSelectorSearch >> searchMethods: aSelectBlock in: anEnvironment [

	^ OrderedCollection streamContents: [ :stream |
		  anEnvironment classesAndTraitsDo: [ :class |
			  class methodsDo: [ :method |
				  (aSelectBlock value: method) ifTrue: [ stream nextPut: method ] ].
			  class classSide methodsDo: [ :method |
				  (aSelectBlock value: method) ifTrue: [ stream nextPut: method ] ] ] ]
]
