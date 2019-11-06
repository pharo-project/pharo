"
I am a model class which is used instead of the typed, untyped and empty NECModel's.
"
Class {
	#name : #CompletionModel,
	#superclass : #Object,
	#instVars : [
		'node',
		'completionToken',
		'entries',
		'sorter'
	],
	#classVars : [
		'SorterClass'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
CompletionModel class >> sorterClass [
	^ SorterClass
]

{ #category : #accessing }
CompletionModel class >> sorterClass: aSorter [

	SorterClass := aSorter
	
]

{ #category : #entries }
CompletionModel >> commonPrefix [
	"Longest match among the typed text and all menu entries"

 	| seed matchLengths prefixLength |
	entries := self entries.
	seed := entries first contents asString.
	matchLengths := entries allButFirst
		collect: [ :entry | seed charactersExactlyMatching: entry contents asString ]
		thenSelect: [ :e | e >= completionToken size ].
	prefixLength := matchLengths isEmpty
		ifTrue: [ 0 ]
		ifFalse: [ matchLengths min ].
	^ seed first: prefixLength.
]

{ #category : #entries }
CompletionModel >> completionAt: aNumber [
	| entry |
	
	entry := (self entries at: aNumber) completion separateKeywords.
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]. 
	
	
]

{ #category : #accessing }
CompletionModel >> completionToken [

 	^ completionToken
]

{ #category : #accessing }
CompletionModel >> entries [
	^entries ifNil: [entries := self initEntries ].
]

{ #category : #entries }
CompletionModel >> entryCount [
	^ self entries size
]

{ #category : #testing }
CompletionModel >> hasEntries [
	^ self entries isNotEmpty
]

{ #category : #testing }
CompletionModel >> hasMessage [
	^ self message notNil
]

{ #category : #entries }
CompletionModel >> initEntries [
	| suggestionsList |
	self sorter: self class sorterClass new.
	suggestionsList := self sortList: node completionEntries.
	^ suggestionsList collect: [ :each | NECEntry contents: each node: node ]
]

{ #category : #initialization }
CompletionModel >> initialize [ 
	completionToken := ''
]

{ #category : #accessing }
CompletionModel >> message [
	^self hasEntries 
		ifFalse: ['no completions found'] 
		ifTrue: [nil]
]

{ #category : #accessing }
CompletionModel >> narrowString [

 	^ completionToken
]

{ #category : #action }
CompletionModel >> narrowWith: aString [ 
	completionToken := aString ifNil: [ '' ].
	entries := entries 
		ifNil: [ self initEntries ]
		ifNotNil: [  entries select: [ :each | each contents beginsWith: completionToken  ] ]

]

{ #category : #accessing }
CompletionModel >> node [
	^ node
]

{ #category : #accessing }
CompletionModel >> node: anObject [
	node := anObject
]

{ #category : #sorting }
CompletionModel >> sortList: aList [
	"this is where the sorting strategy is set"
	^ sorter sortCompletionList: aList
]

{ #category : #sorting }
CompletionModel >> sorter [
	^ sorter
]

{ #category : #sorting }
CompletionModel >> sorter: aSorter [
	"the functionality allowing to hook up the sorter"
	sorter := aSorter
]

{ #category : #accessing }
CompletionModel >> title [
	[
	^ node receiver propertyAt: #type ifPresent: #name ifAbsent: ''
	] on: Error do: [ :e | ^ '' ]
]
