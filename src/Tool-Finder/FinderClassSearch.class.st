"
I implement a search for classes in a given environment.

I am a subclass of ̀FinderSearch̀ and am used by ̀FinderModel̀ to perform searches
for classes.

"
Class {
	#name : #FinderClassSearch,
	#superclass : #FinderSearch,
	#category : #'Tool-Finder-Search'
}

{ #category : #private }
FinderClassSearch >> buildResult: aListOfClasses [

	| result |
	result := Dictionary new.
	aListOfClasses do: [ :each |
		result at: each put: (each selectors sort: [ :a :b | a < b ]) ].
	^ result
]

{ #category : #information }
FinderClassSearch >> name [
	"Returns the name of the search."

	^ 'Classes'
]

{ #category : #searching }
FinderClassSearch >> searchByRegex: aRegex in: anEnvironment [
	"Perform a search given aRegex in anEnvironment."

	^ self buildResult:
		  (self searchClasses: [ :class | aRegex search: class name ] in: anEnvironment)
]

{ #category : #searching }
FinderClassSearch >> searchByString: aString in: anEnvironment [
	"Perform a search given aString in anEnvironment."

	^ self buildResult: (self searchClasses: [ :class |
			   class name includesSubstring: aString caseSensitive: false ] in: anEnvironment)
]

{ #category : #private }
FinderClassSearch >> searchClasses: aSelectBlock in: anEnvironment [

	^ OrderedCollection streamContents: [ :stream |
		  anEnvironment classesAndTraitsDo: [ :class |
			  (aSelectBlock value: class) ifTrue: [ stream nextPut: class ] ] ]
]
