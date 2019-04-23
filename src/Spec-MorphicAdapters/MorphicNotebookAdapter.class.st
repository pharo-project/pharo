"
I'm an adapter to show a notebook (a tab manager)
"
Class {
	#name : #MorphicNotebookAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #'table defaults' }
MorphicNotebookAdapter class >> cellInset [ 

	^ 3
]

{ #category : #factory }
MorphicNotebookAdapter >> addModelTo: aNotebook [
	self model pages ifEmpty: [ ^ self ].
	self model pages
		do: [ :each | 
			"Since I do not have the page added, I need to take it from the list. 
		 But I know this will be the last added :)"
			self addPage: each to: aNotebook ].
	"force first page to be drawn"
	self model selectedPage ifNil: [ self model selectedPageIndex: 1 ].
	aNotebook selectedPageIndex: self model selectedPageIndex.
	aNotebook announcer when: NotebookPageChanged send: #pageChanged: to: self
]

{ #category : #factory }
MorphicNotebookAdapter >> addPage: each to: aNotebook [

	aNotebook addLazyPage: [ self buildContentForPage: each ] label: (self buildLabelForPage: each).
	"Since I do not have the page added, I need to take it from the list. But I know this will be the last added :)"
	aNotebook pages last model: each
]

{ #category : #factory }
MorphicNotebookAdapter >> buildContentForPage: aPage [
	^ aPage retrievePresenter
		ifNotNil: [ :presenter |
			aPage layoutSpec
				ifNil: [ presenter buildWithSpec ]
				ifNotNil: [ :spec | presenter buildWithSpec: spec ] ]
]

{ #category : #factory }
MorphicNotebookAdapter >> buildLabelForPage: aPage [
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
MorphicNotebookAdapter >> buildWidget [
	| notebookMorph |

	notebookMorph := NotebookMorph new
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
MorphicNotebookAdapter >> pageChanged: ann [
	self model selectedPage: ann page model
]

{ #category : #updating }
MorphicNotebookAdapter >> selectPage: aPage [
	self widgetDo: [ :w | w page: (w pageWithModel: aPage) ]
]

{ #category : #updating }
MorphicNotebookAdapter >> updatePageContent: aPage [

	self widgetDo: [ :w |
		(w pageWithModel: aPage) reset.
		"if we are updating the page being displayed, we need to trigger a refresh"
		aPage = self model selectedPage ifTrue: [ 
			w updatePageIndex: (self model selectedPageIndex) ] ]
]

{ #category : #updating }
MorphicNotebookAdapter >> updatePageTitle: aPage [

	self widgetDo: [ :w |
		w 
			relabelPage: (w pageWithModel: aPage) 
			with: (self buildLabelForPage: aPage) ]
]

{ #category : #updating }
MorphicNotebookAdapter >> updatePagesWith: aCollection [
	self
		widgetDo: [ :aNotebook | 
			| pagesToRemove |
			pagesToRemove := aNotebook pages reject: [ :pageMorph | aCollection anySatisfy: [ :page | page = pageMorph model ] ].
			pagesToRemove do: [ :pageMorph | aNotebook removePage: pageMorph ].
			aCollection reject: [ :page | aNotebook hasPageWithModel: page ] thenDo: [ :page | self addPage: page to: aNotebook ] ]
]
