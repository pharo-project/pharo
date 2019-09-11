Class {
	#name : #LazyTabGroupMorph,
	#superclass : #TabGroupMorph,
	#instVars : [
		'announcer',
		'toolbarMorph',
		'headerMorph'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #adding }
LazyTabGroupMorph >> addLazyPage: aBlock label: aStringOrMorph [
	self addLazyPage: aBlock label: aStringOrMorph toolbar: nil
]

{ #category : #adding }
LazyTabGroupMorph >> addLazyPage: aBlock label: aStringOrMorph toolbar: aToolbar [
	self addLazyPage: aBlock label: aStringOrMorph toolbar: aToolbar collapsable: false
]

{ #category : #adding }
LazyTabGroupMorph >> addLazyPage: aBlock label: aStringOrMorph toolbar: aToolbar action: anActionMorphOrNil [
	| newPage label |
	newPage := LazyTabPage new.
	newPage
		lazyPageMorphCreation: aBlock; 
		labelMorph: aStringOrMorph; 
		toolbar: aToolbar.
	label := self 
			buildLabelFrom: aStringOrMorph 
			withAction: anActionMorphOrNil.
	self 
		addPage: newPage
		label: label
]

{ #category : #adding }
LazyTabGroupMorph >> addLazyPage: aBlock label: aStringOrMorph toolbar: aToolbar collapsable: aBoolean [
	| newPage label |
	newPage := LazyTabPage new.
	newPage
		lazyPageMorphCreation: aBlock; 
		labelMorph: aStringOrMorph; 
		toolbar: aToolbar.
	label := self 
			buildLabelFrom: aStringOrMorph 
			withAction: (aBoolean 
				ifTrue: [GLMActionButtonBrick new
					action: [self removePage: newPage];
					icon: self theme windowCloseForm ]
				ifFalse: [nil]).
	self 
		addPage: newPage
		label: label.
	
]

{ #category : #accessing }
LazyTabGroupMorph >> announcer [
	^ announcer ifNil: [announcer := GLMAnnouncer new]
]

{ #category : #private }
LazyTabGroupMorph >> buildLabelFrom: aStringOrMorph withAction: aBrick [
	| label innerLabel |

	innerLabel := aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [
			|container labelMorph|
			container := GLMBrick new.
			labelMorph := GLMLabelBrick new
				text: aStringOrMorph;
				textColor: container themer textSecondaryColor;
			 	hSpaceFill;
			 	vSpaceFill;
				marginLeft: 5.
			container
				addBrickBack: labelMorph;
				hSpaceFill;
				vSpaceFill.
			
			container maxWidth: [ 5 + labelMorph approximateWidth + (aBrick ifNotNil: [ :b | b normalImage width * 1.5 ] ifNil: [ 0 ])].
			container ].
		
	label := aBrick
			ifNil: [ innerLabel ]
			ifNotNil: [ aBrick vSpaceFill; width: (aBrick normalImage width * 1.5). innerLabel addBrickBack: aBrick. innerLabel ].
					
	^ label
]

{ #category : #accessing }
LazyTabGroupMorph >> headerMorph [
	^ headerMorph
]

{ #category : #accessing }
LazyTabGroupMorph >> headerMorph: anObject [
	headerMorph := anObject.
	
	headerMorph clipSubmorphs: true.
]

{ #category : #initialization }
LazyTabGroupMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self removeMorph: self contentMorph;
		removeMorph: self tabSelectorMorph;  
		toolbarMorph: self newToolbarMorph;
		headerMorph: self newHeaderMorph;
		addMorph: self headerMorph;
		addMorph: self contentMorph.
	
"	self
		borderWidth: 0;
		changeTableLayout;
		cellPositioning: #topLeft;
		cellInset: 0 @ -1;
		reverseTableCells: true;
		pageMorphs: OrderedCollection new;
		tabSelectorMorph: self newTabSelectorMorph;
		contentMorph: self newContentMorph;
		addMorph: self tabSelectorMorph;
		addMorph: self contentMorph.
	self tabSelectorMorph addDependent: self"
]

{ #category : #'instance creation' }
LazyTabGroupMorph >> newContentMorph [
	"Answer a new content morph"

	|p|
	p := GLMPanelMorph new
		roundedCorners: #(2 3 4);
		changeTableLayout; 
		layoutInset: 4;
		cellInset: 8;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	p borderStyle: (self theme tabPanelBorderStyleFor: self).
	^p
]

{ #category : #private }
LazyTabGroupMorph >> newHeaderMorph [
	^ (self theme 
			newRowIn: self theme
			for: {self tabSelectorMorph . self toolbarMorph}) cellInset: 0; cellPositioning: #bottomCenter; borderWidth: 0
]

{ #category : #building }
LazyTabGroupMorph >> newTabSelectorMorph [
	"Answer a new tab selector morph"

	^ GLMTabSelectorBrick new
		minHeight: self theme buttonMinHeight;
		hSpaceFill;
		vResizing: #shrinkWrap
]

{ #category : #private }
LazyTabGroupMorph >> newToolbarMorph [
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
LazyTabGroupMorph >> pageAt: index [
	^ (self pages at: index ) actualPageMorph
]

{ #category : #private }
LazyTabGroupMorph >> removePage: aPage [
	| removedPageIndex |
	removedPageIndex := self pages indexOf: aPage.
	self announcer suspendAllWhile: [ super removePage: aPage ].
	self announcer announce: 
		(LazyTabPageRemoved new 
			tabs: self; 
			page: aPage; 
			pageIndex: removedPageIndex;
			newIndex: self tabSelectorMorph selectedIndex).
	self pages isEmpty 
		ifTrue: [ self contentMorph removeAllMorphs ]
]

{ #category : #private }
LazyTabGroupMorph >> removePageIndex: anInteger [
	self removePage: (self pages at: anInteger)
]

{ #category : #accessing }
LazyTabGroupMorph >> toolbarMorph [
	^ toolbarMorph
]

{ #category : #accessing }
LazyTabGroupMorph >> toolbarMorph: anObject [
	toolbarMorph := anObject.
]

{ #category : #updating }
LazyTabGroupMorph >> updatePageAtIndex: anInteger withLabel: aStringOrMorph toolbar: aToolbar action: anActionMorph [
	
	(self pages at: anInteger)
			toolbar: aToolbar.
	self 
		relabelPage: (self pages at: anInteger)
		with: (self 
			buildLabelFrom: aStringOrMorph 
			withAction: anActionMorph).
	(self tabSelectorMorph selectedIndex = anInteger) ifTrue: [  
		self updatePageIndex: anInteger ]

]

{ #category : #private }
LazyTabGroupMorph >> updatePageIndex: index [

	"Change to the given page index, update the toolbar and send the announcement"

	| p oldPage |

	index = 0
		ifTrue: [ ^ self ].
	oldPage := self tabSelectorMorph selectedTab.
	p := self pageMorph.
	p
		ifNil: [ self contentMorph addMorph: ( self pageAt: index ) ]
		ifNotNil: [ | pageBounds |

			pageBounds := self pageMorph bounds.
			self contentMorph replaceSubmorph: p by: ( self pageAt: index ).
			self
				flag: 'Without setting here the bounds the pange moves a few pizels when refreshing the inspector'.
			self pageMorph bounds: pageBounds.
			self pageMorph layoutChanged
			].

	( ( self pages at: index ) toolbar notNil and: [ ( self pages at: index ) toolbar hasSubmorphs ] )
		ifTrue: [ self toolbarMorph hasSubmorphs
				ifFalse: [ self toolbarMorph addMorph: ( self pages at: index ) toolbar ]
				ifTrue: [ self toolbarMorph
						replaceSubmorph: self toolbarMorph submorphs first
						by: ( self pages at: index ) toolbar
					]
			]
		ifFalse: [ self toolbarMorph removeAllMorphs ].

	self headerMorph layoutChanged.
	self pageMorph layoutChanged.
	self adoptPaneColor: ( self owner ifNil: [ self ] ) paneColor.
	( self tabSelectorMorph tabs at: index ) font: self tabSelectorMorph font.
	self announcer
		announce:
			( LazyTabPageChanged new
				tabs: self;
				page: ( self pages at: index );
				oldPage: oldPage;
				pageIndex: index )
]
