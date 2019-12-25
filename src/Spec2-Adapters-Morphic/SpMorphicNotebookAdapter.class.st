"
I'm an adapter to show a notebook (a tab manager)
"
Class {
	#name : #SpMorphicNotebookAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'table defaults' }
SpMorphicNotebookAdapter class >> cellInset [ 

	^ 3
]

{ #category : #initialization }
SpMorphicNotebookAdapter >> addFocusRotationKeyBindings [

	self widget tabSelectorMorph
		bindKeyCombination: Character tab asKeyCombination 
			toAction: [ self focusNext ];
		bindKeyCombination: Character tab shift asKeyCombination 
			toAction: [ self focusPrevious ]
]

{ #category : #factory }
SpMorphicNotebookAdapter >> addModelTo: aNotebook [
	self model pages ifEmpty: [ ^ self ].
	self model pages
		do: [ :each | "Since I do not have the page added, I need to take it from the list. 
		 But I know this will be the last added :)" self addPage: each to: aNotebook ].
	"force first page to be drawn"
	self model selectedPage ifNil: [ self model selectPageIndex: 1 ].
	aNotebook selectedPageIndex: self model selectedPageIndex.
	aNotebook announcer when: SpNotebookPageChanged send: #pageChanged: to: self
]

{ #category : #factory }
SpMorphicNotebookAdapter >> addPage: each to: aNotebook [

	aNotebook 
		addLazyPage: [ self buildContentForPage: each ] 
		label: (self buildLabelForPage: each).
	"Since I do not have the page added, I need to take it from the list. But I know 
	 this will be the last added :)"
	aNotebook pages last model: each
]

{ #category : #factory }
SpMorphicNotebookAdapter >> buildContentForPage: aPage [

	^ aPage retrievePresenter ifNotNil: [ :presenter |
		presenter owner: self presenter.
		aPage layoutSpec
			ifNil: [ presenter buildWithSpec ]
			ifNotNil: [ :spec | presenter buildWithSpec: spec ] ]
]

{ #category : #factory }
SpMorphicNotebookAdapter >> buildLabelForPage: aPage [
	"I cannot use a PanelMorph here (as I would like) because Panels have bg colors setted 
	 from theme (and then... they look always same in the tab)"
	| labelMorph |
	
	labelMorph := Morph new 
		changeTableLayout;
		vResizing: #spaceFill;
		hResizing: #shrinkWrap;
		cellInset: self class cellInset;
		color: Color transparent;
		listDirection: #leftToRight;
		yourself.
		
	aPage icon ifNotNil: [ :icon |
		labelMorph addMorphBack: (icon asMorph) ].
	labelMorph addMorphBack: aPage title asMorph.
	
	^ labelMorph
]

{ #category : #factory }
SpMorphicNotebookAdapter >> buildWidget [
	| notebookMorph |

	notebookMorph := SpNotebookMorph new
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		setBalloonText: self help;
		yourself.
	self addModelTo: notebookMorph.

	self model whenPagesChangedDo: [ :pages | self updatePagesWith: pages ].
	self model whenSelectedPageChangedDo: [ :page | self selectPage: page ].

	^ notebookMorph
]

{ #category : #private }
SpMorphicNotebookAdapter >> pageChanged: ann [
	self model selectPage: ann page model
]

{ #category : #updating }
SpMorphicNotebookAdapter >> selectPage: aPage [
	self widgetDo: [ :w | w page: (w pageWithModel: aPage) ]
]

{ #category : #protocol }
SpMorphicNotebookAdapter >> takeKeyboardFocus [

	self widget tabSelectorMorph takeKeyboardFocus
]

{ #category : #updating }
SpMorphicNotebookAdapter >> updatePageContent: aPage [

	self widgetDo: [ :w |
		(w pageWithModel: aPage) reset.
		"if we are updating the page being displayed, we need to trigger a refresh"
		aPage = self model selectedPage ifTrue: [ 
			w updatePageIndex: (self model selectedPageIndex) ] ]
]

{ #category : #updating }
SpMorphicNotebookAdapter >> updatePageTitle: aPage [

	self widgetDo: [ :w |
		w 
			relabelPage: (w pageWithModel: aPage) 
			with: (self buildLabelForPage: aPage) ]
]

{ #category : #updating }
SpMorphicNotebookAdapter >> updatePagesWith: aCollection [
	| pagesToRemove |
	
	self widget ifNil: [ ^ self ].
		
	pagesToRemove := self widget pages reject: [ :pageMorph | 
		aCollection anySatisfy: [ :page | page = pageMorph model ] ].
	pagesToRemove do: [ :pageMorph | self widget removePage: pageMorph ].
	aCollection 
		reject: [ :page | self widget hasPageWithModel: page ] 
		thenDo: [ :page | self addPage: page to: self widget ]
]
