"
I am a model class which is used instead of the typed, untyped and empty NECModel's.
"
Class {
	#name : #TestCompletionModel,
	#superclass : #Object,
	#instVars : [
		'clazz',
		'node',
		'completionToken',
		'entries'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
TestCompletionModel >> arrowString [

 	^ completionToken
]

{ #category : #accessing }
TestCompletionModel >> clazz [
	^ clazz
]

{ #category : #accessing }
TestCompletionModel >> clazz: anObject [
	clazz := anObject.
	clazz isNil ifTrue: [ clazz := UndefinedObject ]
]

{ #category : #accessing }
TestCompletionModel >> commonPrefix [
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
TestCompletionModel >> completionAt: aNumber [
	| entry |
	
	entry := (self entries at: aNumber) completion separateKeywords.
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]. 
	
	
]

{ #category : #accessing }
TestCompletionModel >> entries [
	^entries ifNil: [entries := self initEntries ].
]

{ #category : #accessing }
TestCompletionModel >> entryCount [
	^ self entries size
]

{ #category : #accessing }
TestCompletionModel >> hasMessage [
	^ self message notNil
]

{ #category : #accessing }
TestCompletionModel >> initEntries [
	| producer temp |
	producer := TestMatchedNodeProducer new.
	producer completionContext: clazz.
	temp := (producer completionListForNode: node) asOrderedCollection. 
	^ temp collect: [ :each | TestCompletionEntry contents: each ]
]

{ #category : #accessing }
TestCompletionModel >> initialize [ 
	completionToken := ''.
	clazz := UndefinedObject
]

{ #category : #accessing }
TestCompletionModel >> isEmpty [ 
	^ self entries isEmpty
]

{ #category : #accessing }
TestCompletionModel >> message [
	^self isEmpty 
		ifTrue: ['no completions found'] 
		ifFalse: [nil]
]

{ #category : #action }
TestCompletionModel >> narrowString [

 	^ completionToken
]

{ #category : #action }
TestCompletionModel >> narrowWith: aString [ 
	completionToken := aString ifNil: [  '' ].
	node narrowWith: completionToken.
	entries := nil
]

{ #category : #accessing }
TestCompletionModel >> node [
	^ node
]

{ #category : #accessing }
TestCompletionModel >> node: anObject [
	node := anObject
]

{ #category : #accessing }
TestCompletionModel >> notEmpty [
	^ self entries isNotEmpty
]

{ #category : #accessing }
TestCompletionModel >> title [
	| type |
	[
	type := node receiver propertyAt: #type ifAbsent: [ ^ '' ].
	^ type name
	] on: Error do: [ :e | ^ '' ]
]
