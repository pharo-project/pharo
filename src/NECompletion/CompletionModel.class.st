"
I am a model class which is used instead of the typed, untyped and empty NECModel's.
"
Class {
	#name : #CompletionModel,
	#superclass : #Object,
	#instVars : [
		'clazz',
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
CompletionModel >> arrowString [

 	^ completionToken
]

{ #category : #accessing }
CompletionModel >> clazz [
	^ clazz
]

{ #category : #accessing }
CompletionModel >> clazz: anObject [
	clazz := anObject
]

{ #category : #accessing }
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

{ #category : #accessing }
CompletionModel >> completionAt: aNumber [
	| entry |
	
	entry := (self entries at: aNumber) completion separateKeywords.
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]. 
	
	
]

{ #category : #accessing }
CompletionModel >> entries [
	^entries ifNil: [entries := self initEntries ].
]

{ #category : #accessing }
CompletionModel >> entryCount [
	^ self entries size
]

{ #category : #accessing }
CompletionModel >> hasMessage [
	^ self message notNil
]

{ #category : #accessing }
CompletionModel >> initEntries [
	| producer suggestionsList |
	producer := CompletionProducer new.
	self sorter: CompletionModel sorter.
	producer completionContext: clazz.
	suggestionsList := self sortList: (producer completionListForNode: node).
	^ suggestionsList collect: [ :each | CompletionEntry contents: each node: node ]
]

{ #category : #accessing }
CompletionModel >> initialize [ 
	completionToken := ''
]

{ #category : #accessing }
CompletionModel >> isEmpty [ 
	^ self entries isEmpty
]

{ #category : #accessing }
CompletionModel >> message [
	^self isEmpty 
		ifTrue: ['no completions found'] 
		ifFalse: [nil]
]

{ #category : #action }
CompletionModel >> narrowString [

 	^ completionToken
]

{ #category : #action }
CompletionModel >> narrowWith: aString [ 
	completionToken := aString ifNil: [  '' ].
	node narrowWith: completionToken.
	entries := nil
]

{ #category : #accessing }
CompletionModel >> node [
	^ node
]

{ #category : #accessing }
CompletionModel >> node: anObject [
	node := anObject
]

{ #category : #accessing }
CompletionModel >> notEmpty [
	^ self entries isNotEmpty
]

{ #category : #accessing }
CompletionModel >> sortList: aList [
	"this is where the sorting strategy is set"
	^ sorter sortCompletionList: aList asOrderedCollection
]

{ #category : #accessing }
CompletionModel >> sorter [
	^ sorter
]

{ #category : #accessing }
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
