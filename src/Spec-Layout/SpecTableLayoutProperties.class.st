Class {
	#name : #SpecTableLayoutProperties,
	#superclass : #SpecLayoutProperties,
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
	#category : #Spec-Layout
}

{ #category : #'table defaults' }
SpecTableLayoutProperties >> cellInset [
	^cellInset
]

{ #category : #accessing }
SpecTableLayoutProperties >> cellInset: aNumber [
	cellInset := aNumber
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> cellPositioning [
	^cellPositioning
]

{ #category : #accessing }
SpecTableLayoutProperties >> cellPositioning: aSymbol [
	cellPositioning := aSymbol
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> cellSpacing [
	^cellSpacing
]

{ #category : #accessing }
SpecTableLayoutProperties >> cellSpacing: aSymbol [
	cellSpacing := aSymbol.
]

{ #category : #testing }
SpecTableLayoutProperties >> includesTableProperties [
	^true
]

{ #category : #initialization }
SpecTableLayoutProperties >> initialize [
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
SpecTableLayoutProperties >> layoutInset [
	^layoutInset
]

{ #category : #accessing }
SpecTableLayoutProperties >> layoutInset: aNumber [
	layoutInset := aNumber
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> listCentering [
	^listCentering
]

{ #category : #accessing }
SpecTableLayoutProperties >> listCentering: aSymbol [
	listCentering := aSymbol
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> listDirection [
	^listDirection
]

{ #category : #accessing }
SpecTableLayoutProperties >> listDirection: aSymbol [
	listDirection := aSymbol.
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> listSpacing [
	^listSpacing
]

{ #category : #accessing }
SpecTableLayoutProperties >> listSpacing: aSymbol [
	listSpacing := aSymbol
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> maxCellSize [
	^maxCellSize
]

{ #category : #accessing }
SpecTableLayoutProperties >> maxCellSize: aNumber [
	maxCellSize := aNumber.
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> minCellSize [
	^minCellSize
]

{ #category : #accessing }
SpecTableLayoutProperties >> minCellSize: aNumber [
	minCellSize := aNumber.
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> reverseTableCells [
	^reverseTableCells
]

{ #category : #accessing }
SpecTableLayoutProperties >> reverseTableCells: aBool [
	reverseTableCells := aBool
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> rubberBandCells [
	^rubberBandCells
]

{ #category : #accessing }
SpecTableLayoutProperties >> rubberBandCells: aBool [
	rubberBandCells := aBool.
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> wrapCentering [
	^wrapCentering
]

{ #category : #accessing }
SpecTableLayoutProperties >> wrapCentering: aSymbol [
	wrapCentering := aSymbol
]

{ #category : #'table defaults' }
SpecTableLayoutProperties >> wrapDirection [
	^wrapDirection
]

{ #category : #accessing }
SpecTableLayoutProperties >> wrapDirection: aSymbol [
	wrapDirection := aSymbol
]
