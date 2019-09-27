"
This is the History slot. It saves (by default 5) older values. 
The complete history can be read with #readHistory:
"
Class {
	#name : #HistorySlot,
	#superclass : #IndexedSlot,
	#instVars : [
		'size'
	],
	#category : #'VariablesLibrary-Slots'
}

{ #category : #'code generation' }
HistorySlot >> emitValue: aMethodBuilder [
	"for speed, we inline the #last send instead of calling the #read: method"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder send: #last
]

{ #category : #initialization }
HistorySlot >> initialize [
	size := 5

]

{ #category : #initialization }
HistorySlot >> initialize: anObject [
	super write: (OrderedCollection with: nil) to: anObject

]

{ #category : #printing }
HistorySlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' size: ';
		store: size
]

{ #category : #'meta-object-protocol' }
HistorySlot >> rawRead: anObject [
	"return the wrapper (the collection) used to store the value"
	^super read: anObject
]

{ #category : #'meta-object-protocol' }
HistorySlot >> read: anObject [
	"we return the last value from the collection"
	^ (self rawRead: anObject) last
]

{ #category : #'meta-object-protocol' }
HistorySlot >> readHistory: anObject [
	"return the complete history"
	^ self rawRead: anObject
]

{ #category : #accessing }
HistorySlot >> size [
	^ size
]

{ #category : #accessing }
HistorySlot >> size: anObject [
	size := anObject
]

{ #category : #'meta-object-protocol' }
HistorySlot >> wantsInitialization [

    ^ true
]

{ #category : #'meta-object-protocol' }
HistorySlot >> write: aDictionary to: anObject [
	| history |
	history := self rawRead: anObject.
	history size >= size
		ifTrue: [ history removeFirst ].
	^ history addLast: aDictionary
]
