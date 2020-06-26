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
	#classVars : [
		'SpaceAfterCompletion'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
NECEntry class >> contents: aString node: aNode [
	^ self new contents: aString node: aNode
]

{ #category : #accessing }
NECEntry class >> spaceAfterCompletion [
	
	^ SpaceAfterCompletion
]

{ #category : #accessing }
NECEntry class >> spaceAfterCompletion: aBoolean [

	SpaceAfterCompletion := aBoolean
]

{ #category : #operations }
NECEntry >> <= aECEntry [ 
	^ contents <= aECEntry contents
]

{ #category : #operations }
NECEntry >> activateOn: aCompletionContext [

	| entryContents |
	"By default insert the contents of the entry instead of the token in the text editor"
	entryContents := self contents asSymbol separateKeywords.
	entryContents := (self class spaceAfterCompletion
				ifTrue: [ entryContents , ' ' ]
				ifFalse: [ entryContents ]).
	aCompletionContext replaceTokenInEditorWith: entryContents
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
