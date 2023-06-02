Class {
	#name : #FinderSelectorsSearchStrategy,
	#superclass : #FinderSearchStrategy,
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderSelectorsSearchStrategy class >> strategyName [

	^ #Selectors
]

{ #category : #actions }
FinderSelectorsSearchStrategy >> constructDictionary [

	| result |
	result := Dictionary new.
	self messagesNameSearch do: [ :method |
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

{ #category : #testing }
FinderSelectorsSearchStrategy >> isSelectorsSearch [

	^ true
]

{ #category : #actions }
FinderSelectorsSearchStrategy >> messageSearchBlock [

	| exactMatch aString |
	aString := finder searchingString.
	exactMatch := aString first = $" and: [ aString last = $" ].

	exactMatch ifFalse: [ ^ [ :method | method selector includesSubstring: aString caseSensitive: false ] ].

	^ (Symbol findInterned: (aString copyFrom: 2 to: aString size - 1)) ifNotNil: [ :aSymbol | [ :method | method selector = aSymbol ] ]
]

{ #category : #actions }
FinderSelectorsSearchStrategy >> messagesNameSearch [
	"I'm searching for selectors"

	^ finder useRegEx
		  ifTrue: [
			  | regex |
			  regex := finder searchingString asRegex.
			  self methodSearch: [ :method | regex search: method selector asString ] ]
		  ifFalse: [ self methodSearch: self messageSearchBlock ]
]
