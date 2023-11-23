"
I am a Finder search mode used when looking for methods in the system.
"
Class {
	#name : 'FinderSelectorsSearchStrategy',
	#superclass : 'FinderSearchStrategy',
	#category : 'Tool-Finder-Base',
	#package : 'Tool-Finder',
	#tag : 'Base'
}

{ #category : 'accessing' }
FinderSelectorsSearchStrategy class >> strategyName [

	^ #Selectors
]

{ #category : 'actions' }
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

{ #category : 'accessing' }
FinderSelectorsSearchStrategy >> findSelectedClassIn: path [

	^ path first isSingle
		  ifTrue: [ path first itemMethod methodClass ]
		  ifFalse: [ (path at: 2 ifAbsent: [ nil ]) ifNotNil: [ :node | node item ] ]
]

{ #category : 'accessing' }
FinderSelectorsSearchStrategy >> findSelectedMethodIn: path [

	^ path first isSingle
		  ifTrue: [ path first itemMethod selector ]
		  ifFalse: [ path first item ]
]

{ #category : 'actions' }
FinderSelectorsSearchStrategy >> messageSearchBlock [

	| exactMatch aString |
	aString := finder searchingString.
	exactMatch := aString first = $" and: [ aString last = $" ].

	exactMatch ifFalse: [ ^ [ :method | method selector includesSubstring: aString caseSensitive: false ] ].

	^ (Symbol findInterned: (aString copyFrom: 2 to: aString size - 1)) ifNotNil: [ :aSymbol | [ :method | method selector = aSymbol ] ]
]

{ #category : 'actions' }
FinderSelectorsSearchStrategy >> messagesNameSearch [
	"I'm searching for selectors"

	^ finder useRegEx
		  ifTrue: [
			  | regex |
			  regex := finder searchingString asRegex.
			  self methodSearch: [ :method | regex search: method selector asString ] ]
		  ifFalse: [ self methodSearch: self messageSearchBlock ]
]
