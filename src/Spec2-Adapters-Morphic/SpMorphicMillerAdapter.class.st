"
I'm an adapter for Miller Layouts.
A paned layout will split (horizontally or vertically) panels (in fixed yet proportional spaces), 
and it will scroll the panels that exceed SpMillerLayout>>#visiblePages property.  
"
Class {
	#name : #SpMorphicMillerAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#instVars : [
		'innerWidget',
		'lastExtent',
		'lastPageSize',
		'needRecalculatePages'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #accessing }
SpMorphicMillerAdapter >> add: aPresenter [
 
	self add: aPresenter constraints: SpLayoutConstraints new
]

{ #category : #private }
SpMorphicMillerAdapter >> add: aPresenter constraints: constraints [

	self widget ifNil: [ ^ self ].

	aPresenter owner: self model. 
	needRecalculatePages := true.
	self 
		basicAdd: aPresenter
		constraints: constraints 
		to: innerWidget.
	self scrollToShowLastPage
]

{ #category : #private }
SpMorphicMillerAdapter >> addConstraints: constraints toChild: childMorph [
	"adds constraits by child"

	"Set morph to stay rigid"
	layout direction setRigidityOfNonExpandedMorph: childMorph.
	
	^ childMorph
]

{ #category : #private }
SpMorphicMillerAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [

	| childMorph |
	childMorph := self buildMorph: aPresenter constraints: constraints.	
	aPanel addMorphBack: childMorph.
	
	^ childMorph
]

{ #category : #accessing }
SpMorphicMillerAdapter >> childrenWidgets [
	
	^ innerWidget submorphs
]

{ #category : #factory }
SpMorphicMillerAdapter >> disableHScrollBar [
	
	widget hScrollbarShowNever
]

{ #category : #factory }
SpMorphicMillerAdapter >> enableHScrollBar [
	
	widget hScrollbarShowWhenNeeded
]

{ #category : #private }
SpMorphicMillerAdapter >> forceRecalculatePages [

	needRecalculatePages := true.
	self recalculatePages
]

{ #category : #private }
SpMorphicMillerAdapter >> initialize [

	super initialize.
	needRecalculatePages := false

]

{ #category : #factory }
SpMorphicMillerAdapter >> layout: aLayout [

	layout := aLayout.
	
	widget ifNil: [
		"build of widget was differed up to here (to know what is the direction)"
		innerWidget := aLayout direction newWidgetOn: self.
		innerWidget borderWidth: aLayout borderWidth.
		innerWidget cellInset: aLayout spacing.
		
		layout whenVisiblePagesChangedDo: [ self forceRecalculatePages ].
		
		widget := self newScrollPaneFor: innerWidget.
		self presenter hasHorizontalScrollBar 
			ifFalse: [ self disableHScrollBar ].
		self presenter whenHorizontalScrollBarChangedDo: [ :show |
			show 
				ifTrue: [ self enableHScrollBar ]
			 	ifFalse: [ widget hScrollbarShowNever ] ] ].

	super layout: aLayout
]

{ #category : #factory }
SpMorphicMillerAdapter >> newHorizontal [

	^ self newPanel 
		listDirection: #leftToRight;
		yourself
]

{ #category : #factory }
SpMorphicMillerAdapter >> newPanel [

	^ super newPanel
		extent: 1@1;
		yourself
]

{ #category : #factory }
SpMorphicMillerAdapter >> newScrollPaneFor: aWidget [

	^ MillerScrollPane new
		  changeScrollerTableLayout;
		  hResizing: #spaceFill;
		  vResizing: #spaceFill;
		  color: Color transparent;
		  borderColor: Color transparent;
		  scrollTarget: aWidget;
		  onLayoutChange: [ self recalculatePages ];
		  yourself
]

{ #category : #factory }
SpMorphicMillerAdapter >> newVertical [

	^ self newPanel 
		listDirection: #topToBottom;
		yourself
]

{ #category : #private }
SpMorphicMillerAdapter >> recalculatePageHeights [

	| newHeight widgets height |
	height := self widget height.
	widgets := self childrenWidgets.
	newHeight := widgets size = 1
		ifTrue: [ height ]
		ifFalse: [ height / (layout visiblePages min: widgets size) - (layout spacing / (layout visiblePages min: widgets size)) ].
		
	widgets reverseWithIndexDo: [ :each :index |
		each 
			top: (index - 1) * newHeight;
			height: newHeight ]
]

{ #category : #private }
SpMorphicMillerAdapter >> recalculatePageWidths [

	| newWidth widgets width |
	widgets := self childrenWidgets.
	width := self widget width.
	newWidth := widgets size = 1
		ifTrue: [ width ]
		ifFalse: [ (width / (layout visiblePages min: widgets size)) - (layout spacing / (layout visiblePages min: widgets size)) ].
		
	widgets reverseWithIndexDo: [ :each :index |
		each 
			left: (index - 1) * newWidth;
			width: newWidth ]
]

{ #category : #private }
SpMorphicMillerAdapter >> recalculatePages [

	self widget ifNil: [ ^ self ].
	self childrenWidgets ifEmpty: [ ^ self ].
	(needRecalculatePages not 
		and: [ lastExtent = self widget extent 
		and: [ lastPageSize = self childrenWidgets size ] ]) 
		ifTrue: [ ^ self ].

	needRecalculatePages := false.
	lastExtent := self widget extent.
	lastPageSize := self childrenWidgets size.
	layout direction recalculatePages: self
]

{ #category : #accessing }
SpMorphicMillerAdapter >> remove: aPresenter [

	| morph |
	needRecalculatePages := true.
	morph := aPresenter adapter widget.	
	innerWidget removeMorph: morph
]

{ #category : #accessing }
SpMorphicMillerAdapter >> removeAll [

	needRecalculatePages := true.
	innerWidget removeAllMorphs
]

{ #category : #factory }
SpMorphicMillerAdapter >> removeSubWidgets [

	innerWidget removeAllMorphs
]

{ #category : #paginator }
SpMorphicMillerAdapter >> scrollByDeltaPercent: percent [

	| millerWidth millerDelta |	
	millerWidth := self widget scroller localSubmorphBounds width.
	millerDelta := millerWidth * (percent / 100).
	self widget scroller offset: (self widget scroller offset x + millerDelta)@0	
]

{ #category : #private }
SpMorphicMillerAdapter >> scrollToShowLastPage [

	self widget defer: [ | size firstMorphIndex |
		size := innerWidget submorphs size.
		firstMorphIndex := size - self presenter visiblePages + 1. 
		self widget showMorph: (innerWidget submorphs at: ((firstMorphIndex max: 1) min: size)) ]
]

{ #category : #accessing }
SpMorphicMillerAdapter >> selectPage: aNumber [

	self widget defer: [
		(self childrenWidgets at: aNumber ifAbsent: [ nil ])
			ifNotNil: [ :aMorph | self widget showMorph: aMorph ] ]
]
