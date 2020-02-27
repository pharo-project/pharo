"
A SpecTableLayout is a variant of spec layout that uses TableLayout layout policy.
"
Class {
	#name : #SpecTableLayout,
	#superclass : #Object,
	#instVars : [
		'commands',
		'selector',
		'layoutProperties'
	],
	#category : #'Spec-Layout-Layouts'
}

{ #category : #'instance creation' }
SpecTableLayout class >> column [
	^ self new listDirection: #topToBottom
]

{ #category : #'instance creation' }
SpecTableLayout class >> row [
	^ self new listDirection: #leftToRight
]

{ #category : #commands }
SpecTableLayout >> add: aSpec [ 
	commands add: (SpecTableLayoutAdd subwidget: aSpec) 
]

{ #category : #commands }
SpecTableLayout >> add: modelGetter withSpec: aSymbol [ 
	commands add: (SpecTableLayoutAddWithSpec subwidget: modelGetter spec: aSymbol) 
]

{ #category : #commands }
SpecTableLayout >> add: modelGetter withSpec: aSymbol wrapped: aBlock [
	| wrapper |
	wrapper := SpecWrapperTableLayout wrap: modelGetter withSpec: aSymbol.
	aBlock value: wrapper.
	self add: wrapper
	
]

{ #category : #commands }
SpecTableLayout >> add: aSpec wrapped: aBlock [
	| wrapper |
	wrapper := SpecWrapperTableLayout wrap: aSpec.
	aBlock value: wrapper.
	self add: wrapper
	
]

{ #category : #commands }
SpecTableLayout >> addSpacer [
	self addSpacerWeighted: 1 
]

{ #category : #commands }
SpecTableLayout >> addSpacerSized: aNumber [
	commands add: (SpecTableLayoutAddSpacer sized: aNumber) 
]

{ #category : #commands }
SpecTableLayout >> addSpacerWeighted: aNumber [
	commands add: (SpecTableLayoutAddSpacer weighted: aNumber) 
]

{ #category : #converting }
SpecTableLayout >> asArray [
	commands 
		select: [ :c | c isKindOf: SpecTableLayoutAddSpacer ] 
		thenDo: [ :c | c orientation: self orientation ].
	
	^ { #TableContainerPresenter . #layoutProperties: . layoutProperties } , 
	  (commands flatCollect: #asSpecElements)
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> cellInset: aNumber [
	layoutProperties cellInset: aNumber
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> cellPositioning: aSymbol [
	layoutProperties cellPositioning: aSymbol
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> cellSpacing: aSymbol [
	layoutProperties cellSpacing: aSymbol
]

{ #category : #accessing }
SpecTableLayout >> commands [
	^ commands
]

{ #category : #accessing }
SpecTableLayout >> commands: anObject [
	commands := anObject
]

{ #category : #private }
SpecTableLayout >> createColumn: aBlock [
	| column |
	column := SpecTableLayout column.
	aBlock value: column.
	^ column
]

{ #category : #private }
SpecTableLayout >> createRow: aBlock [
	| row |
	row :=  SpecTableLayout row.
	aBlock value: row.
	^ row
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> hResizing: aSymbol [
	layoutProperties hResizing: aSymbol
]

{ #category : #commands }
SpecTableLayout >> height: aNumber [
	"Doesn't work when opened in window, only when part of another layout"
	self send: #height: withArguments: {aNumber}
]

{ #category : #initialization }
SpecTableLayout >> initialize [ 
	super initialize.
	
	commands := OrderedCollection new.
	layoutProperties := SpecTableLayoutProperties new.
	layoutProperties 
		reverseTableCells: true;
		hResizing: #spaceFill;
		vResizing: #spaceFill.
]

{ #category : #testing }
SpecTableLayout >> isSpecLayout [

	^ true
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> layoutInset: aNumber [
	layoutProperties layoutInset: aNumber
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> listCentering: aSymbol [
	layoutProperties listCentering: aSymbol
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> listDirection: aSymbol [
	layoutProperties listDirection: aSymbol
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> listSpacing: aSymbol [
	layoutProperties listSpacing: aSymbol
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> maxCellSize: aNumber [
	layoutProperties maxCellSize: aNumber
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> minCellSize: aNumber [
	layoutProperties minCellSize: aNumber
]

{ #category : #commands }
SpecTableLayout >> minHeight: aNumber [
	self send: #minHeight: withArguments: {aNumber}
]

{ #category : #commands }
SpecTableLayout >> minWidth: aNumber [
	self send: #minWidth: withArguments: {aNumber}
]

{ #category : #commands }
SpecTableLayout >> newColumn: aBlock [
	self add: (self createColumn: aBlock)
]

{ #category : #commands }
SpecTableLayout >> newColumn: aBlock width: aNumber [
	self add: ((self createColumn: aBlock) width: aNumber)
]

{ #category : #commands }
SpecTableLayout >> newRow: aBlock [
	self add: (self createRow: aBlock)
]

{ #category : #commands }
SpecTableLayout >> newRow: aBlock height: aNumber [
	self add: ((self createRow: aBlock) height: aNumber)
]

{ #category : #private }
SpecTableLayout >> orientation [
	| dir |
	dir := layoutProperties listDirection.
	^ ((dir == #topToBottom) | (dir == #bottomToTop))
		ifTrue: [ #vertical ]
		ifFalse: [ #horizontal ]
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> reverseTableCells: aBool [
	layoutProperties reverseTableCells: aBool
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> rubberBandCells: aBool [
	layoutProperties rubberBandCells: aBool
]

{ #category : #accessing }
SpecTableLayout >> selector [
	^ selector
]

{ #category : #accessing }
SpecTableLayout >> selector: anObject [
	selector := anObject
]

{ #category : #commands }
SpecTableLayout >> send: aSelector [
	self send: aSelector withArguments: #()
]

{ #category : #commands }
SpecTableLayout >> send: aSelector withArguments: arguments [
	commands 
		add: (SpecTableLayoutSend
					selector: aSelector
					arguments: arguments)
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> vResizing: aSymbol [
	layoutProperties vResizing: aSymbol
]

{ #category : #commands }
SpecTableLayout >> width: aNumber [
	"Doesn't work when opened in window, only when part of another layout"
	self send: #width: withArguments: {aNumber}
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> wrapCentering: aSymbol [
	layoutProperties wrapCentering: aSymbol
]

{ #category : #'commands - layout properties' }
SpecTableLayout >> wrapDirection: aSymbol [
	layoutProperties wrapDirection: aSymbol
]
