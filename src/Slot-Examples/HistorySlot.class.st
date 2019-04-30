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
	#category : #'Slot-Examples-Base'
}

{ #category : #initialization }
HistorySlot >> initialize [
	size := 5

]

{ #category : #initialization }
HistorySlot >> initialize: anObject [
	super write: (OrderedCollection with: nil) to: anObject.

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
HistorySlot >> read: anObject [

	^ (super read: anObject) last
]

{ #category : #'meta-object-protocol' }
HistorySlot >> readHistory: anObject [
	^ (super read: anObject)
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
HistorySlot >> wantsInitalization [

    ^ true
]

{ #category : #'meta-object-protocol' }
HistorySlot >> write: aDictionary to: anObject [
	| history |
	history := super read: anObject.
	history size >= size
		ifTrue: [ history removeFirst ].
	^ history addLast: aDictionary
]
