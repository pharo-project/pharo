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
		'fetcher',
		'displayedContents'
	],
	#classVars : [
		'OriginDisplayed',
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
NECEntry class >> originDisplayed: aBoolean [

	OriginDisplayed := aBoolean
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
	^ contents
]

{ #category : 'accessing' }
NECEntry >> contents: aString [
	contents := aString
]

{ #category : 'accessing' }
NECEntry >> contents: aString node: aNode [

	contents := aString.
	node := aNode
]

{ #category : 'detail information' }
NECEntry >> createDescription [
	
	^ NECEntryDescription label: self contents
]

{ #category : 'operations' }
NECEntry >> description [

	^ description ifNil: [ description := self createDescription ]
]

{ #category : 'accessing' }
NECEntry >> displayedContents [
	"displayedContents offers the possibility to display a different information in the menu than the one returned when the user select an entry. By default displayContents is delegating to contents."

	^ displayedContents ifNil: [ self contents ]
]

{ #category : 'accessing' }
NECEntry >> displayedContents: aString [ 
	"displayedContents offers the possibility to display a different information in the menu than the one returned when the user select an entry. By default displayContents is delegating to contents."

	displayedContents := aString
]

{ #category : 'accessing' }
NECEntry >> fetcher [

	^ fetcher
]

{ #category : 'accessing' }
NECEntry >> fetcher: anObject [
	"We store the name of the fetcher as a string 
	to ease debugging when a dynamic fetcher returns a item from another one more static."
	
	fetcher := anObject class name
]

{ #category : 'accessing' }
NECEntry >> hightlightSymbol [
	^nil
]

{ #category : 'accessing' }
NECEntry >> label [ 

	| typeString |
	typeString := self rawLabel.
	self originDisplayed ifTrue: [ typeString := typeString , ' from: ', self fetcher asString ].
	^ typeString
]

{ #category : 'accessing' }
NECEntry >> node [

	^ node
]

{ #category : 'accessing' }
NECEntry >> node: anObject [

	node := anObject
]

{ #category : 'accessing' }
NECEntry >> originDisplayed [

	^ OriginDisplayed ifNil: [ OriginDisplayed := false ]
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

{ #category : 'accessing' }
NECEntry >> rawLabel [
	^ 'unknown'
]
