Class {
	#name : #GLMMorphicSingleSpotter,
	#superclass : #DialogWindow,
	#instVars : [
		'textMorph',
		'text',
		'listMorph',
		'listValues',
		'listLabels',
		'listIndex',
		'answer',
		'parent'
	],
	#category : #Glamour-Morphic-Renderer-Spotter
}

{ #category : #'instance creation' }
GLMMorphicSingleSpotter class >> on: aRequest [
	^ self basicNew initializeOn: aRequest
]

{ #category : #'instance creation' }
GLMMorphicSingleSpotter class >> on: aRequest parent: parent [
	^ self basicNew initializeOn: aRequest parent: parent
]

{ #category : #'instance creation' }
GLMMorphicSingleSpotter class >> openOn: aRequest [
	^ UITheme builder openModal: (self on: aRequest)
]

{ #category : #accessing }
GLMMorphicSingleSpotter >> answer [
	"Answer the result of the dialog or nil, if this dialog has been cancelled."
	
	^ answer
]

{ #category : #callbacks }
GLMMorphicSingleSpotter >> arrowKey: anEvent from: aMorph [
	"Ignore this request."
]

{ #category : #actions }
GLMMorphicSingleSpotter >> cancel [
	answer := nil.
	^ super cancel
]

{ #category : #callbacks }
GLMMorphicSingleSpotter >> clickIconAt: anInteger [
	"Ignore this request."
]

{ #category : #morphic }
GLMMorphicSingleSpotter >> defaultFocusMorph [
	^ textMorph
]

{ #category : #morphic }
GLMMorphicSingleSpotter >> deselect [
	self listIndex: 0.  
]

{ #category : #callbacks }
GLMMorphicSingleSpotter >> doubleClick [
	self ok
]

{ #category : #callbacks }
GLMMorphicSingleSpotter >> editorKeyStroke: anEvent [
	anEvent keyCharacter = Character arrowDown ifTrue: [
		self listIndex: (self listIndex + 1 min: listValues size).
		^ true ].
	anEvent keyCharacter = Character arrowUp ifTrue: [
		self listIndex: (self listIndex - 1 max: 1). 
		^ true ].
	^ false
]

{ #category : #callbacks }
GLMMorphicSingleSpotter >> iconAt: anIndex [
	^ model ifNotNil: [model iconFor: (listValues at: anIndex ifAbsent: [ ^ nil ])]
]

{ #category : #initialization }
GLMMorphicSingleSpotter >> initializeOn: aRequest [
	model := aRequest.
	listValues := listLabels := #().
	listIndex := 0.
	self initialize.
	self title: aRequest prompt.
	self text: aRequest default

]

{ #category : #initialization }
GLMMorphicSingleSpotter >> initializeOn: aRequest parent: theParent [
	parent := theParent.
	self initializeOn: aRequest.

]

{ #category : #testing }
GLMMorphicSingleSpotter >> isAssisted [
	^ self model assisted
]

{ #category : #testing }
GLMMorphicSingleSpotter >> isEnabled [
	^ self model assisted
		ifTrue: [ self text isEmptyOrNil not ]
		ifFalse: [ self listIndex > 0 ]
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listIndex [
	^ listIndex
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listIndex: anInteger [
	anInteger isZero not ifTrue: [
		parent selectionIn: self.].
	listIndex = anInteger
		ifTrue: [ ^ self ].
	listIndex := anInteger.
	self listSelection ifNotNil: [ :selection |
		self isAssisted ifTrue: [ 
			text := self model labelFor: self listSelection.
			self changed: #text ] ].
	self changed: #listIndex; changed: #isEnabled
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listLabels [
	^ listLabels
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listSelection [
	^ listValues at: self listIndex ifAbsent: [ nil ]
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listSelection: anObject [

	self listIndex: (listValues indexOf: anObject ifAbsent: [ 0 ])
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listValues [
	^ listValues
]

{ #category : #accessing-list }
GLMMorphicSingleSpotter >> listValues: aCollection [
	| previous |
	previous := self listSelection.
	listValues := aCollection asArray.
	listLabels := listValues
		collect: [ :each | model labelFor: each ].
	self changed: #listLabels; listSelection: previous
]

{ #category : #morphic }
GLMMorphicSingleSpotter >> newContentMorph [
	^ (self newColumn: { self newTextMorph. self newListMorph })
		minWidth: 320;
		minHeight: 200;
		yourself
]

{ #category : #morphic }
GLMMorphicSingleSpotter >> newListMorph [
	listMorph := PluggableListMorph
		on: self
		list: #listLabels
		selected: #listIndex
		changeSelected: #listIndex:.
	listMorph
		borderStyle: (self theme listNormalBorderStyleFor: listMorph);
		color: (self theme listNormalFillStyleFor: listMorph);
		cornerStyle: self preferredCornerStyle;
		doubleClickSelector: #doubleClick;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselect: false.
	^ listMorph
]

{ #category : #morphic }
GLMMorphicSingleSpotter >> newOKButton [
	^ self newOKButtonFor: self getEnabled: #isEnabled
]

{ #category : #morphic }
GLMMorphicSingleSpotter >> newTextMorph [
	textMorph := self
		newTextEntryFor: self
		getText: #text
		setText: #text:
		help: nil.
	textMorph
		autoAccept: true;
		selectAll.
	textMorph textMorph
		on: #keyStroke send: #editorKeyStroke: to: self.
	^ textMorph
]

{ #category : #actions }
GLMMorphicSingleSpotter >> ok [
	answer := self model assisted
		ifTrue: [ self text ]
		ifFalse: [ self listSelection ].
	^ super ok
]

{ #category : #accessing }
GLMMorphicSingleSpotter >> parent [
	^ parent
]

{ #category : #accessing }
GLMMorphicSingleSpotter >> parent: anObject [
	parent := anObject
]

{ #category : #accessing-text }
GLMMorphicSingleSpotter >> text [
	^ text
]

{ #category : #accessing-text }
GLMMorphicSingleSpotter >> text: aString [
	text = aString
		ifTrue: [ ^ self ].
	text := aString.
	self listValues: (self model valuesFor: text).
	self isAssisted ifFalse: [
		self listIndex: (self listLabels size = 1
			ifTrue: [ 1 ]
			ifFalse: [ self listLabels findFirst: [ :each | each sameAs: text ] ]) ].
	self changed: #text; changed: #isEnabled
]
