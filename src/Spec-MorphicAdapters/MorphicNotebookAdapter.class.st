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
	self model pages do: [ :each |
		aNotebook 
			addLazyPage: [ self buildContentForPage: each ] 
			label: (self buildLabelForPage: each).
		"Since I do not have the page added, I need to take it from the list. 
		 But I know this will be the last added :)"
		aNotebook pages last model: each ].
	"force first page to be drawn"
	self model selectedPage ifNil: [ 
		self model selectedPageIndex: 1 ].
	aNotebook selectedPageIndex: self model selectedPageIndex. 
	aNotebook announcer 
		when: NotebookPageChanged 
		send: #pageChanged:
		to: self
]

{ #category : #factory }
MorphicNotebookAdapter >> buildContentForPage: aPage [
		
	^ aPage retrievePresenter ifNotNil: #buildWithSpec
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
		yourself.
	self addModelTo: notebookMorph.

	^ notebookMorph
]

{ #category : #private }
MorphicNotebookAdapter >> pageChanged: ann [

	self model basicSelectedPage: ann page model
]

{ #category : #updating }
MorphicNotebookAdapter >> selectedPage [
	
	self widgetDo: [ :w |
		w page: (w pageWithModel: self model selectedPage) ]
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
