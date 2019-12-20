"
I represent a completion entry that is management by a ECModel and shown in the ECMenuMorph as a menu entry.

Each of my subclass will represent a specific kind of entity. For example a globalvariable, self...
"
Class {
	#name : #NECEntry,
	#superclass : #Object,
	#instVars : [
		'contents',
		'node',
		'description'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
NECEntry class >> contents: aString node: aNode [
	^ self new contents: aString node: aNode
]

{ #category : #operations }
NECEntry >> <= aECEntry [ 
	^ contents <= aECEntry contents
]

{ #category : #private }
NECEntry >> browse [
	self subclassResponsibility
]

{ #category : #accessing }
NECEntry >> contents [
	^contents
]

{ #category : #accessing }
NECEntry >> contents: aString node: aNode [
	contents := aString.
	node := aNode
]

{ #category : #'detail information' }
NECEntry >> createDescription [
	self subclassResponsibility
]

{ #category : #operations }
NECEntry >> description [
	^description ifNil: [ description := self createDescription ]
]

{ #category : #accessing }
NECEntry >> hightlightSymbol [
	^nil
]

{ #category : #accessing }
NECEntry >> label [
	^ 'unknown'
]

{ #category : #printing }
NECEntry >> printOn: aStream [ 
	aStream 
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: contents;
		nextPut: $,;
		nextPutAll: self hightlightSymbol;
		nextPut: $)
]
