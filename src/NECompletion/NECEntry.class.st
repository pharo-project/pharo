"
I represent a completion entry that is management by a ECModel and shown in the ECMenuMorph as a menu entry.

Each of my subclass will represent a specific kind of entity. For example a globalvariable, self...
"
Class {
	#name : #NECEntry,
	#superclass : #Object,
	#instVars : [
		'contents',
		'type',
		'description'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
NECEntry class >> contents: aString [ 
	^ self new setContents: aString
]

{ #category : #'instance creation' }
NECEntry class >> contents: aString type: aSymbol [
	^ self new setContents: aString type: aSymbol
]

{ #category : #operations }
NECEntry >> <= aECEntry [ 
	^ contents <= aECEntry contents
]

{ #category : #'ui related' }
NECEntry >> browseWith: anECContext [ 
	| guessType |
	guessType := self guessTypeWith: anECContext.
	guessType ifNil: [^ false].
	Smalltalk tools browser openOnClass: guessType.
	^ true
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
	clazz := self guessTypeWith: anECContext.
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
	description ifNotNil: [ ^ description ].
	^ description := self createDescriptionWith: anECContext
]

{ #category : #operations }
NECEntry >> guessTypeWith: anECContext [ 
	^ nil
]

{ #category : #testing }
NECEntry >> isInstance [
	^ false
]

{ #category : #testing }
NECEntry >> isLocal [
	^ false
]

{ #category : #testing }
NECEntry >> isSelector [
	^ false
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
		nextPutAll: self type;
		nextPut: $)
]

{ #category : #accessing }
NECEntry >> setContents: aString [ 
	contents := aString.
]

{ #category : #accessing }
NECEntry >> setContents: aString type: aSymbol [ 
	contents := aString.
	type := aSymbol
]

{ #category : #accessing }
NECEntry >> type [
	^type
]
