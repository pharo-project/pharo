"
This class provides a compact bit encoding for the most commonly used layout properties.
"
Class {
	#name : #LayoutProperties,
	#superclass : #Object,
	#instVars : [
		'hResizing',
		'vResizing',
		'disableLayout'
	],
	#category : #'Morphic-Base-Layouts'
}

{ #category : #converting }
LayoutProperties >> asTableLayoutProperties [
	^(TableLayoutProperties new)
		hResizing: self hResizing;
		vResizing: self vResizing;
		disableTableLayout: self disableTableLayout;
		yourself
]

{ #category : #'table defaults' }
LayoutProperties >> cellInset [
	"Default"
	^0
]

{ #category : #'table defaults' }
LayoutProperties >> cellPositioning [
	^#center
]

{ #category : #'table defaults' }
LayoutProperties >> cellSpacing [
	"Default"
	^#none
]

{ #category : #accessing }
LayoutProperties >> disableTableLayout [
	^disableLayout
]

{ #category : #accessing }
LayoutProperties >> disableTableLayout: aBool [
	disableLayout := aBool
]

{ #category : #accessing }
LayoutProperties >> hResizing [
	^hResizing
]

{ #category : #accessing }
LayoutProperties >> hResizing: aSymbol [
	hResizing := aSymbol
]

{ #category : #testing }
LayoutProperties >> includesTableProperties [
	^false
]

{ #category : #initialization }
LayoutProperties >> initialize [
	super initialize.
	hResizing := vResizing := #rigid.
	disableLayout := false.
]

{ #category : #initialize }
LayoutProperties >> initializeFrom: defaultProvider [
	"Initialize the receiver from a default provider"
	self hResizing: defaultProvider hResizing.
	self vResizing: defaultProvider vResizing.
	self disableTableLayout: defaultProvider disableTableLayout.
]

{ #category : #'table defaults' }
LayoutProperties >> layoutInset [
	^0
]

{ #category : #'table defaults' }
LayoutProperties >> listCentering [
	"Default"
	^#topLeft
]

{ #category : #'table defaults' }
LayoutProperties >> listDirection [
	"Default"
	^#topToBottom
]

{ #category : #'table defaults' }
LayoutProperties >> listSpacing [
	"Default"
	^#none
]

{ #category : #'table defaults' }
LayoutProperties >> maxCellSize [
	^SmallInteger maxVal
]

{ #category : #'table defaults' }
LayoutProperties >> minCellSize [
	^0
]

{ #category : #'table defaults' }
LayoutProperties >> reverseTableCells [
	^false
]

{ #category : #'table defaults' }
LayoutProperties >> rubberBandCells [
	^false
]

{ #category : #accessing }
LayoutProperties >> vResizing [
	^vResizing
]

{ #category : #accessing }
LayoutProperties >> vResizing: aSymbol [
	vResizing := aSymbol
]

{ #category : #'table defaults' }
LayoutProperties >> wrapCentering [
	^#topLeft
]

{ #category : #'table defaults' }
LayoutProperties >> wrapDirection [
	^#none
]
