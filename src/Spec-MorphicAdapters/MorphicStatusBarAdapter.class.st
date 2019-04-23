"
I'm the adapter used to bridge a StatusBarPresenter and its morphic representation.
"
Class {
	#name : #MorphicStatusBarAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'pushedMessages'
	],
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #accessing }
MorphicStatusBarAdapter class >> defaultHeight [

	^ 25
]

{ #category : #accessing }
MorphicStatusBarAdapter class >> defaultItemSeparation [

	^ 5
]

{ #category : #factory }
MorphicStatusBarAdapter >> addModelTo: aMorph [

	self model message ifNotNil: [ :message |
		self pushMessage: message to: aMorph ]
]

{ #category : #factory }
MorphicStatusBarAdapter >> buildWidget [
	| panelMorph |
	
	panelMorph := PanelMorph new 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: self class defaultItemSeparation;
		height: self class defaultHeight;
		yourself.

	self addModelTo: panelMorph.
	
	^ panelMorph
]

{ #category : #initialization }
MorphicStatusBarAdapter >> initialize [ 

	super initialize.
	pushedMessages := OrderedCollection new
]

{ #category : #api }
MorphicStatusBarAdapter >> popMessage [

	self widgetDo: [ :w |
		self popMessageFrom: w ]
]

{ #category : #private }
MorphicStatusBarAdapter >> popMessageFrom: aMorph [
	| message |

	pushedMessages ifEmpty: [ ^ self ].
	pushedMessages removeLast.
	message := pushedMessages 
		ifNotEmpty: #last
		ifEmpty: [ '' ].
			
	aMorph 
		removeAllMorphs;
		addMorph: message asStringMorph
]

{ #category : #api }
MorphicStatusBarAdapter >> pushMessage [

	self widgetDo: [ :w |
		self 
			pushMessage: self model message 
			to: w ]
]

{ #category : #private }
MorphicStatusBarAdapter >> pushMessage: message to: aMorph [

	pushedMessages add: message.
	aMorph 
		removeAllMorphs;
		addMorph: message asStringMorph
]

{ #category : #updating }
MorphicStatusBarAdapter >> update: aSymbol [ 

	aSymbol = #pushMessage ifTrue: [ self pushMessage ].
	aSymbol = #popMessage ifTrue: [ self popMessage ].

	^ super update: aSymbol
]
