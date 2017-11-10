"
Specially themed label used for tab selector items. 
"
Class {
	#name : #TabLabelMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'tabSelector',
		'normalLabel',
		'selectedLabel'
	],
	#category : #Morphic-Widgets-PolyTabs
}

{ #category : #accessing }
TabLabelMorph >> adoptPaneColor: paneColor [
	"Use the theme for fillStyle and border."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self borderStyle: self borderStyleToUse
]

{ #category : #accessing }
TabLabelMorph >> borderStyleToUse [
	"Answer the borderStyle that should be used for the receiver."
	
	^self isSelected
		ifTrue: [self theme tabLabelSelectedBorderStyleFor: self]
		ifFalse: [self theme tabLabelNormalBorderStyleFor: self]
]

{ #category : #geometry }
TabLabelMorph >> extent: aPoint [
	"Set the extent of the receiver.
	Update the pane color for size dependent fill/border styles."

	super extent: aPoint.
	self adoptPaneColor
]

{ #category : #accessing }
TabLabelMorph >> fillStyleToUse [
	"Answer the basic fill style for the receiver."

	^self isSelected
		ifTrue: [self selectedFillStyle] 
		ifFalse: [self normalFillStyle]
]

{ #category : #accessing }
TabLabelMorph >> focusBounds [
	"Answer the bounds for drawing the focus indication."

	^(self bounds width < 6 or: [self bounds height < 6])
		ifTrue: [super focusBounds]
		ifFalse: [super focusBounds insetBy: (2@2 corner: 2@0)]
]

{ #category : #accessing }
TabLabelMorph >> font: aFont [
	"Set the label font"

	self normalLabel allMorphsDo: [:m |
		(m respondsTo: #font:) ifTrue: [m font: aFont]].
	self selectedLabel allMorphsDo: [:m |
		(m respondsTo: #font:) ifTrue: [m font: aFont]]
]

{ #category : #initialization }
TabLabelMorph >> initialize [
	"Initialize the receiver."
	
	super initialize.
	self fillStyle: self fillStyleToUse.
	self borderStyle: self borderStyleToUse
]

{ #category : #testing }
TabLabelMorph >> isSelected [
	"Answer whether the tab is selected."

	^self tabSelector
		ifNil: [false]
		ifNotNil: [:selector | selector selectedTab == self]
]

{ #category : #testing }
TabLabelMorph >> isVisible [
	"Answer whether the tab is currently visible."

	^self tabSelector
		ifNil: [false]
		ifNotNil: [:selector | selector visibleTabs includes: self]
]

{ #category : #accessing }
TabLabelMorph >> label [
	"Answer the label contents."

	^self isSelected
		ifTrue: [self selectedLabel userString ifNil: [self firstSubmorph]]
		ifFalse: [self normalLabel userString ifNil: [self firstSubmorph]]
]

{ #category : #accessing }
TabLabelMorph >> label: aStringOrMorph [
	"Change the label contents"

	|l|
	l := aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [(self theme buttonLabelForText: aStringOrMorph)
					vResizing: #shrinkWrap;
					hResizing: #shrinkWrap].
	self
		normalLabel: l;
		selectedLabel: l;
		selected: self isSelected
]

{ #category : #accessing }
TabLabelMorph >> label: aStringOrMorph selected: selectedStringOrMorph [
	"Change the label contents"

	|l s|
	l := aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [(self theme buttonLabelForText: aStringOrMorph)
					vResizing: #shrinkWrap;
					hResizing: #shrinkWrap].
	s:= selectedStringOrMorph isMorph
		ifTrue: [selectedStringOrMorph]
		ifFalse: [(self theme buttonLabelForText: selectedStringOrMorph)
					vResizing: #shrinkWrap;
					hResizing: #shrinkWrap].
	self
		normalLabel: l;
		selectedLabel: s;
		selected: self isSelected
]

{ #category : #layout }
TabLabelMorph >> minWidth [
	"Consult the theme also."

	^super minWidth max: self theme buttonMinWidth
]

{ #category : #style }
TabLabelMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme tabLabelNormalFillStyleFor: self
]

{ #category : #accessing }
TabLabelMorph >> normalLabel [

	^ normalLabel
]

{ #category : #accessing }
TabLabelMorph >> normalLabel: anObject [

	normalLabel := anObject
]

{ #category : #accessing }
TabLabelMorph >> selected: aBoolean [
	"Use the appropriate label."

	self removeAllMorphs.
	self isSelected
		ifTrue: [self addMorph: self selectedLabel]
		ifFalse: [self addMorph: self normalLabel]
]

{ #category : #style }
TabLabelMorph >> selectedFillStyle [
	"Return the selected fillStyle of the receiver."
	
	^self theme tabLabelSelectedFillStyleFor: self
]

{ #category : #accessing }
TabLabelMorph >> selectedLabel [

	^ selectedLabel
]

{ #category : #accessing }
TabLabelMorph >> selectedLabel: anObject [

	selectedLabel := anObject
]

{ #category : #accessing }
TabLabelMorph >> tabSelector [

	^ tabSelector
]

{ #category : #accessing }
TabLabelMorph >> tabSelector: anObject [

	tabSelector := anObject
]
