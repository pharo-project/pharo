Class {
	#name : #SpecLayoutProperties,
	#superclass : #Object,
	#instVars : [
		'hResizing',
		'vResizing',
		'disableLayout'
	],
	#category : 'Spec-Layout'
}

{ #category : #converting }
SpecLayoutProperties >> asTableLayoutProperties [
	^(SpecTableLayoutProperties new)
		hResizing: self hResizing;
		vResizing: self vResizing;
		disableTableLayout: self disableTableLayout;
		yourself
]

{ #category : #'table defaults' }
SpecLayoutProperties >> cellInset [
	"Default"
	^0
]

{ #category : #'table defaults' }
SpecLayoutProperties >> cellPositioning [
	^#center
]

{ #category : #'table defaults' }
SpecLayoutProperties >> cellSpacing [
	"Default"
	^#none
]

{ #category : #accessing }
SpecLayoutProperties >> disableTableLayout [
	^disableLayout
]

{ #category : #accessing }
SpecLayoutProperties >> disableTableLayout: aBool [
	disableLayout := aBool
]

{ #category : #accessing }
SpecLayoutProperties >> hResizing [
	^hResizing
]

{ #category : #accessing }
SpecLayoutProperties >> hResizing: aSymbol [
	hResizing := aSymbol
]

{ #category : #testing }
SpecLayoutProperties >> includesTableProperties [
	^false
]

{ #category : #initialization }
SpecLayoutProperties >> initialize [
	super initialize.
	hResizing := vResizing := #rigid.
	disableLayout := false.
]

{ #category : #initialize }
SpecLayoutProperties >> initializeFrom: defaultProvider [
	"Initialize the receiver from a default provider"
	self hResizing: defaultProvider hResizing.
	self vResizing: defaultProvider vResizing.
	self disableTableLayout: defaultProvider disableTableLayout.
]

{ #category : #'table defaults' }
SpecLayoutProperties >> layoutInset [
	^0
]

{ #category : #'table defaults' }
SpecLayoutProperties >> listCentering [
	"Default"
	^#topLeft
]

{ #category : #'table defaults' }
SpecLayoutProperties >> listDirection [
	"Default"
	^#topToBottom
]

{ #category : #'table defaults' }
SpecLayoutProperties >> listSpacing [
	"Default"
	^#none
]

{ #category : #'table defaults' }
SpecLayoutProperties >> maxCellSize [
	^SmallInteger maxVal
]

{ #category : #'table defaults' }
SpecLayoutProperties >> minCellSize [
	^0
]

{ #category : #'table defaults' }
SpecLayoutProperties >> reverseTableCells [
	^false
]

{ #category : #'table defaults' }
SpecLayoutProperties >> rubberBandCells [
	^false
]

{ #category : #accessing }
SpecLayoutProperties >> vResizing [
	^vResizing
]

{ #category : #accessing }
SpecLayoutProperties >> vResizing: aSymbol [
	vResizing := aSymbol
]

{ #category : #'table defaults' }
SpecLayoutProperties >> wrapCentering [
	^#topLeft
]

{ #category : #'table defaults' }
SpecLayoutProperties >> wrapDirection [
	^#none
]
