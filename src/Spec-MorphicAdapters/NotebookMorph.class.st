Class {
	#name : #NotebookMorph,
	#superclass : #TabGroupMorph,
	#instVars : [
		'headerMorph'
	],
	#category : #'Spec-MorphicAdapters-Notebook'
}

{ #category : #accessing }
NotebookMorph class >> tabCellInset [

	^ 2
]

{ #category : #adding }
NotebookMorph >> addLazyPage: aBlock label: aStringOrMorph [

	self 
		addLazyPage: aBlock 
		label: aStringOrMorph 
		collapsable: false
]

{ #category : #adding }
NotebookMorph >> addLazyPage: aBlock label: aStringOrMorph action: anActionMorphOrNil [
	| newPage label |
	
	newPage := NotebookPageMorph new.
	newPage lazyPageMorphCreation: aBlock.
		
	label := self 
		buildLabelFrom: aStringOrMorph 
		withAction: anActionMorphOrNil.
	
	newPage labelMorph: label.
	
	self addPage: newPage label: label
]

{ #category : #adding }
NotebookMorph >> addLazyPage: aBlock label: aStringOrMorph collapsable: aBoolean [
	| newPage label |
	
	newPage := NotebookPageMorph new.
	newPage lazyPageMorphCreation: aBlock.

	label := self 
		buildLabelFrom: aStringOrMorph 
		withAction: (aBoolean 
			ifTrue: [ self newCloseButtonFor: newPage ]
			ifFalse: [ nil ]).
	
	newPage labelMorph: label.
	
	self addPage: newPage label: label
]

{ #category : #private }
NotebookMorph >> buildLabelFrom: aString [ 
	
	^ LabelMorph newLabel: aString
]

{ #category : #private }
NotebookMorph >> buildLabelFrom: aStringOrMorph withAction: aMorph [
	| innerLabel |

	innerLabel := aStringOrMorph isMorph
		ifTrue: [ aStringOrMorph ]
		ifFalse: [ self buildLabelFrom: aStringOrMorph ].
	
	^ aMorph
		ifNotNil: [ 
			aMorph vResizing: #spaceFill. 
			PanelMorph new 
				addMorphBack: innerLabel;			
				addMorphBack: aMorph; 
				yourself ]
		ifNil: [ 
			innerLabel ]
]

{ #category : #testing }
NotebookMorph >> hasPageWithModel: anObject [
	"an utility to easy know if the notebook has a page for a model"

	^ self pages anySatisfy: [ :each | each model = anObject ]
]

{ #category : #accessing }
NotebookMorph >> headerMorph [
	^ headerMorph
]

{ #category : #accessing }
NotebookMorph >> headerMorph: anObject [
	headerMorph := anObject.
	
	headerMorph clipSubmorphs: true.
]

{ #category : #initialization }
NotebookMorph >> initialize [

	super initialize.
	self 
		removeMorph: self contentMorph;
		removeMorph: self tabSelectorMorph;  
		headerMorph: self newHeaderMorph;
		addMorph: self headerMorph;
		addMorph: self contentMorph
]

{ #category : #factory }
NotebookMorph >> newCloseButtonFor: aPage [

	^ ButtonPresenter new 
		icon: (self theme windowCloseForm);
		action: [ self removePage: aPage ];
		buildWithSpec
]

{ #category : #factory }
NotebookMorph >> newContentMorph [
	"Answer a new content morph"

	^ PanelMorph new
		roundedCorners: #(2 3 4);
		changeTableLayout; 
		layoutInset: 4;
		cellInset: 8;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		borderStyle: (self theme tabPanelBorderStyleFor: self);
		yourself
]

{ #category : #private }
NotebookMorph >> newHeaderMorph [

	^ (self theme newRowIn: self theme for: { self tabSelectorMorph }) 
		cellInset: 0; 
		cellPositioning: #bottomCenter; 
		borderWidth: 0
]

{ #category : #factory }
NotebookMorph >> newTabSelectorMorph [ 

	^ NotebookTabSelectorMorph new
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		cellInset: self class tabCellInset;
		yourself
]

{ #category : #private }
NotebookMorph >> newToolbarMorph [
	^ GLMPanelMorph new
		fillStyle: (SolidFillStyle color: Color transparent);
		borderStyle: (BorderStyle width: 0);
		vResizing: #spaceFill;
		hResizing: #shrinkWrap;
		cellPositioning: #center;
		changeTableLayout;
		cellInset: 0;
		yourself 
]

{ #category : #private }
NotebookMorph >> pageAt: index [
	^ (self pages at: index ) actualPageMorph
]

{ #category : #accessing }
NotebookMorph >> pageWithModel: anObject [
	"an utility to easy find pages with a model"

	^ self pages detect: [ :each | each model = anObject ]
]

{ #category : #private }
NotebookMorph >> removePage: aPage [
	| removedPageIndex |
	removedPageIndex := self pages indexOf: aPage.

	self suspendAnnouncementsDuring: [ super removePage: aPage ].

	self announcer
		announce:
			(LazyTabPageRemoved new
				tabs: self;
				page: aPage;
				pageIndex: removedPageIndex;
				newIndex: self tabSelectorMorph selectedIndex).
	self pages isEmpty ifTrue: [ self contentMorph removeAllMorphs ]
]

{ #category : #private }
NotebookMorph >> removePageIndex: anInteger [
	self removePage: (self pages at: anInteger)
]

{ #category : #private }
NotebookMorph >> suspendAnnouncementsDuring: aBlock [
	| subscriptions |
	subscriptions := self announcer subscriptions subscriptions.
	subscriptions do: [ :sub | self announcer unsubscribe: sub ].
	aBlock value.
	subscriptions do: [ :sub | self announcer basicSubscribe: sub ]
]

{ #category : #private }
NotebookMorph >> updatePageIndex: index [
	"Change to the given page index, update the toolbar and send the announcement"
	| page oldPage |
	
	index = 0 ifTrue: [ ^ self ].
	
	oldPage := self tabSelectorMorph selectedTab.
	page := self pageMorph.
	page isNil
		ifTrue: [
			self contentMorph addMorph: (self pageAt: index).]
		ifFalse: [ | pageBounds |
			pageBounds := self pageMorph bounds.
			self contentMorph replaceSubmorph: page by: (self pageAt: index).
			self flag: 'Without setting here the bounds the pange moves a few pizels when refreshing the inspector'.
			self pageMorph bounds: pageBounds.
			self pageMorph layoutChanged ].
		
	self headerMorph layoutChanged.
	self pageMorph layoutChanged.
	self adoptPaneColor: (self owner ifNil: [self]) paneColor.
	(self tabSelectorMorph tabs at: index) font: self tabSelectorMorph font.
	
	self announcer announce: (NotebookPageChanged new 
		page: (self pages at: index); 
		oldPage: oldPage; 
		pageIndex: index)
]
