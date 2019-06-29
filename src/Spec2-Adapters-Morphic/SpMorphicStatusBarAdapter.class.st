"
I'm the adapter used to bridge a StatusBarPresenter and its morphic representation.
"
Class {
	#name : #SpMorphicStatusBarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'pushedMessages'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpMorphicStatusBarAdapter class >> defaultHeight [

	^ 25
]

{ #category : #accessing }
SpMorphicStatusBarAdapter class >> defaultItemSeparation [

	^ 5
]

{ #category : #factory }
SpMorphicStatusBarAdapter >> addModelTo: aMorph [

	self model message ifNotNil: [ :message |
		self pushMessage: message to: aMorph ]
]

{ #category : #factory }
SpMorphicStatusBarAdapter >> buildWidget [
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
SpMorphicStatusBarAdapter >> initialize [ 

	super initialize.
	pushedMessages := OrderedCollection new
]

{ #category : #api }
SpMorphicStatusBarAdapter >> popMessage [

	self widgetDo: [ :w |
		self popMessageFrom: w ]
]

{ #category : #private }
SpMorphicStatusBarAdapter >> popMessageFrom: aMorph [
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
SpMorphicStatusBarAdapter >> pushMessage [

	self widgetDo: [ :w |
		self 
			pushMessage: self model message 
			to: w ]
]

{ #category : #private }
SpMorphicStatusBarAdapter >> pushMessage: message to: aMorph [

	pushedMessages add: message.
	aMorph 
		removeAllMorphs;
		addMorph: message asStringMorph
]

{ #category : #updating }
SpMorphicStatusBarAdapter >> update: aSymbol [ 

	aSymbol = #pushMessage ifTrue: [ self pushMessage ].
	aSymbol = #popMessage ifTrue: [ self popMessage ].

	^ super update: aSymbol
]
