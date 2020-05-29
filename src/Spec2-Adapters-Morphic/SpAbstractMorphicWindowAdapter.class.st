Class {
	#name : #SpAbstractMorphicWindowAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

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
	self model hasToolbar ifTrue: [ self addToolBarTo: containerMorph ].
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
SpAbstractMorphicWindowAdapter >> addToolBarTo: aMorph [
	| toolbarMorph |

	toolbarMorph := self model toolBar buildWithSpec.
	aMorph addMorphBack: toolbarMorph.	
	toolbarMorph
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #private }
SpAbstractMorphicWindowAdapter >> newContainerMorph [
		
	^ PanelMorph new
		layoutPolicy: TableLayout new; 
		hResizing: #spaceFill;
		vResizing: #spaceFill;		
		listDirection: #topToBottom; 
		yourself
]
