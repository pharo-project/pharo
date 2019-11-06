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
NECEntry class >> contents: aString [ 
	^ self new setContents: aString
]

{ #category : #'instance creation' }
NECEntry class >> contents: aString node: aNode [
	^ self new setContents: aString node: aNode
]

{ #category : #operations }
NECEntry >> <= aECEntry [ 
	^ contents <= aECEntry contents
]

{ #category : #'ui related' }
NECEntry >> browseWith: anECContext [ 
	| class |
	"no really correct, we need the class of the node..."
	class := node methodNode compilationContext getClass.
	Smalltalk tools browser openOnClass: class.
	^true
]

{ #category : #accessing }
NECEntry >> completion [
	^ self contents asSymbol
]

{ #category : #accessing }
NECEntry >> contents [
	^contents
]

{ #category : #'detail information' }
NECEntry >> createDescriptionWith: anECContext [ 
	| clazz |
	"no really correct, we need the class of the node..."
	clazz := node methodNode compilationContext getClass.
	^ clazz 
		ifNil: [ NECEntryDescription label: self label ]
		ifNotNil: 
			[ NECEntryDescription 
				label: self label
				title: clazz printString
				description: clazz comment ]
]

{ #category : #operations }
NECEntry >> descriptionWith: anECContext [ 
	^description ifNil: [ description := self createDescriptionWith: anECContext ]
]

{ #category : #operations }
NECEntry >> guessTypeWith: aContext [
	"senders of this need to get the type from the AST"
	^nil
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

{ #category : #accessing }
NECEntry >> setContents: aString [ 
	contents := aString.
]

{ #category : #accessing }
NECEntry >> setContents: aString node: aNode [
	contents := aString.
	node := aNode
]
