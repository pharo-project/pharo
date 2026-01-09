"
I represent a completion entry that is management by a ECModel and shown in the ECMenuMorph as a menu entry.

Each of my subclass will represent a specific kind of entity. For example a globalvariable, self...
"
Class {
	#name : 'NECEntry',
	#superclass : 'Object',
	#instVars : [
		'contents',
		'node',
		'description',
		'fetcher'
	],
	#classVars : [
		'SpaceAfterCompletion'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'instance creation' }
NECEntry class >> contents: aString node: aNode [
	^ self new contents: aString node: aNode
]

{ #category : 'accessing' }
NECEntry class >> spaceAfterCompletion [

	^ SpaceAfterCompletion
]

{ #category : 'accessing' }
NECEntry class >> spaceAfterCompletion: aBoolean [

	SpaceAfterCompletion := aBoolean
]

{ #category : 'operations' }
NECEntry >> <= aECEntry [
	^ contents <= aECEntry contents
]

{ #category : 'operations' }
NECEntry >> activateOn: aCompletionContext [

	| entryContents |
	"Log the usage"
	NECUsageTracker incrementUsageFor: self contents.
	
	"By default insert the contents of the entry instead of the token in the text editor"
	entryContents := self contents asSymbol separateKeywords.
	entryContents := (self class spaceAfterCompletion
				ifTrue: [ entryContents , ' ' ]
				ifFalse: [ entryContents ]).
	aCompletionContext replaceTokenInEditorWith: entryContents
]

{ #category : 'matching' }
NECEntry >> beginsWith: aString [

	^ self contents beginsWith: aString
]

{ #category : 'private' }
NECEntry >> browse [
	self subclassResponsibility
]

{ #category : 'accessing' }
NECEntry >> contents [
	^contents
]

{ #category : 'accessing' }
NECEntry >> contents: aString node: aNode [
	contents := aString.
	node := aNode
]

{ #category : 'detail information' }
NECEntry >> createDescription [
	self subclassResponsibility
]

{ #category : 'operations' }
NECEntry >> description [

	^ description ifNil: [ description := self createDescription ]
]

{ #category : 'accessing' }
NECEntry >> fetcher [

	^ fetcher
]

{ #category : 'accessing' }
NECEntry >> fetcher: anObject [

	fetcher := anObject
]

{ #category : 'accessing' }
NECEntry >> hightlightSymbol [
	^nil
]

{ #category : 'accessing' }
NECEntry >> label [
	^ 'unknown'
]

{ #category : 'accessing' }
NECEntry >> node [

	^ node
]

{ #category : 'accessing' }
NECEntry >> node: anObject [

	node := anObject
]

{ #category : 'printing' }
NECEntry >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: contents;
		nextPut: $,.

	self hightlightSymbol ifNotNil: [ aStream nextPutAll: self hightlightSymbol ].

	aStream nextPut: $)
]
