"
I am a model class which is used instead of the typed, untyped and empty NECModel's.
"
Class {
	#name : #CompletionModel,
	#superclass : #Object,
	#instVars : [
		'class',
		'node',
		'completionToken',
		'entries',
		'sorter'
	],
	#classVars : [
		'Sorter'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
CompletionModel class >> sorter [
	^ Sorter
]

{ #category : #accessing }
CompletionModel class >> sorter: aSorter [

	Sorter := aSorter
	
]

{ #category : #accessing }
CompletionModel >> class: aClass [
	class := aClass.
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
CompletionModel >> hasMessage [
	^ self message notNil
]

{ #category : #entries }
CompletionModel >> initEntries [
	| producer suggestionsList |
	producer := CompletionProducerVisitor new.
	self sorter: self class sorter.
	producer completionContext: class.
	suggestionsList := self sortList: (producer completionListForNode: node).
	^ suggestionsList collect: [ :each | CompletionEntry contents: each node: node ]
]

{ #category : #initialization }
CompletionModel >> initialize [ 
	completionToken := ''
]

{ #category : #testing }
CompletionModel >> isEmpty [ 
	^ self entries isEmpty
]

{ #category : #accessing }
CompletionModel >> message [
	^self isEmpty 
		ifTrue: ['no completions found'] 
		ifFalse: [nil]
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

{ #category : #testing }
CompletionModel >> notEmpty [
	^ self entries isNotEmpty
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
	| type |
	[
	type := node receiver propertyAt: #type ifAbsent: [ ^ '' ].
	^ type name
	] on: Error do: [ :e | ^ '' ]
]
