Class {
	#name : #TableLayoutProperties,
	#superclass : #LayoutProperties,
	#instVars : [
		'cellInset',
		'cellPositioning',
		'cellSpacing',
		'layoutInset',
		'listCentering',
		'listDirection',
		'listSpacing',
		'reverseTableCells',
		'rubberBandCells',
		'wrapCentering',
		'wrapDirection',
		'minCellSize',
		'maxCellSize'
	],
	#category : #'Kernel-BasicObjects'
}

{ #category : #'table defaults' }
TableLayoutProperties >> cellInset [
	^cellInset
]

{ #category : #accessing }
TableLayoutProperties >> cellInset: aNumber [
	cellInset := aNumber
]

{ #category : #'table defaults' }
TableLayoutProperties >> cellPositioning [
	^cellPositioning
]

{ #category : #accessing }
TableLayoutProperties >> cellPositioning: aSymbol [
	cellPositioning := aSymbol
]

{ #category : #'table defaults' }
TableLayoutProperties >> cellSpacing [
	^cellSpacing
]

{ #category : #accessing }
TableLayoutProperties >> cellSpacing: aSymbol [
	cellSpacing := aSymbol.
]

{ #category : #testing }
TableLayoutProperties >> includesTableProperties [
	^true
]

{ #category : #initialization }
TableLayoutProperties >> initialize [
	super initialize.
	cellSpacing := listSpacing := wrapDirection := #none.
	cellPositioning := #center.
	listCentering := wrapCentering := #topLeft.
	listDirection := #topToBottom.
	reverseTableCells := rubberBandCells := false.
	layoutInset := cellInset := minCellSize := 0.
	maxCellSize := 1073741823. "SmallInteger maxVal"

]

{ #category : #'table defaults' }
TableLayoutProperties >> layoutInset [
	^layoutInset
]

{ #category : #accessing }
TableLayoutProperties >> layoutInset: aNumber [
	layoutInset := aNumber
]

{ #category : #'table defaults' }
TableLayoutProperties >> listCentering [
	^listCentering
]

{ #category : #accessing }
TableLayoutProperties >> listCentering: aSymbol [
	listCentering := aSymbol
]

{ #category : #'table defaults' }
TableLayoutProperties >> listDirection [
	^listDirection
]

{ #category : #accessing }
TableLayoutProperties >> listDirection: aSymbol [
	listDirection := aSymbol.
]

{ #category : #'table defaults' }
TableLayoutProperties >> listSpacing [
	^listSpacing
]

{ #category : #accessing }
TableLayoutProperties >> listSpacing: aSymbol [
	listSpacing := aSymbol
]

{ #category : #'table defaults' }
TableLayoutProperties >> maxCellSize [
	^maxCellSize
]

{ #category : #accessing }
TableLayoutProperties >> maxCellSize: aNumber [
	maxCellSize := aNumber.
]

{ #category : #'table defaults' }
TableLayoutProperties >> minCellSize [
	^minCellSize
]

{ #category : #accessing }
TableLayoutProperties >> minCellSize: aNumber [
	minCellSize := aNumber.
]

{ #category : #'table defaults' }
TableLayoutProperties >> reverseTableCells [
	^reverseTableCells
]

{ #category : #accessing }
TableLayoutProperties >> reverseTableCells: aBool [
	reverseTableCells := aBool
]

{ #category : #'table defaults' }
TableLayoutProperties >> rubberBandCells [
	^rubberBandCells
]

{ #category : #accessing }
TableLayoutProperties >> rubberBandCells: aBool [
	rubberBandCells := aBool.
]

{ #category : #'table defaults' }
TableLayoutProperties >> wrapCentering [
	^wrapCentering
]

{ #category : #accessing }
TableLayoutProperties >> wrapCentering: aSymbol [
	wrapCentering := aSymbol
]

{ #category : #'table defaults' }
TableLayoutProperties >> wrapDirection [
	^wrapDirection
]

{ #category : #accessing }
TableLayoutProperties >> wrapDirection: aSymbol [
	wrapDirection := aSymbol
]
