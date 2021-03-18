"
I am the adapter used to bridge window presenters and windows or worlds
"
Class {
	#name : #SpAbstractMorphicWindowAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'toolbarMorph'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #testing }
SpAbstractMorphicWindowAdapter class >> isAbstract [ 

	^self name = #SpAbstractMorphicWindowAdapter
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> add: containerMorph toWindow: aSpecWindow [

	aSpecWindow 
		addMorph: containerMorph 
		fullFrame: LayoutFrame identity
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> addContent: aMorph toWindow: aSpecWindow [
	| containerMorph |
	
	containerMorph := self newContainerMorph.
	containerMorph layoutFrame: aMorph layoutFrame.

	"add all decorations (menu, toolbar and statusbar)"
	self model hasMenu ifTrue: [ self addMenuTo: containerMorph ].

	self setToolbarTo: containerMorph.

	"Register for changes in the toolbar"
	self model whenToolbarChangedDo: [ self setToolbarTo: containerMorph ].

	containerMorph addMorphBack: aMorph.
	aMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self model hasStatusBar ifTrue: [ self addStatusBarTo: containerMorph ].
	
	self add: containerMorph toWindow: aSpecWindow
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> addMenuTo: aMorph [

	| menuMorph |
	menuMorph := self model menu buildWithSpec.
	aMorph addMorphBack: menuMorph.	
	menuMorph
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> addStatusBarTo: aMorph [

	| statusBarMorph |
	statusBarMorph := self model statusBar buildWithSpec.
	aMorph addMorphBack: statusBarMorph.	
	statusBarMorph
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> newContainerMorph [

	^ Morph new
		color: Color transparent;
		layoutPolicy: TableLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		listDirection: #topToBottom;
		yourself
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> setToolbarTo: aMorph [

	| newToolbarMorph |
	
	self model hasToolbar 
		ifFalse: [ 
			toolbarMorph ifNotNil: [ 
				aMorph removeMorph: toolbarMorph.
				toolbarMorph := nil.  ].
			^ self. ].
	
	newToolbarMorph := self presenter toolbar buildWithSpec.
	newToolbarMorph
		hResizing: #spaceFill;
		vResizing: #rigid.
	
	"If we have a toolbar morph, we replace with the new one"
	toolbarMorph 
		ifNil: [	aMorph addMorph: newToolbarMorph ]
		ifNotNil: [ aMorph replaceSubmorph: toolbarMorph by: newToolbarMorph ].
		
	toolbarMorph := newToolbarMorph

]
