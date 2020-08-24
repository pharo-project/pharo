Class {
	#name : #AbstractReporter,
	#superclass : #Model,
	#instVars : [
		'categories',
		'categoriesSelected',
		'categoryList',
		'sourceTextModel'
	],
	#category : #'Tool-SystemReporter'
}

{ #category : #'instance creation' }
AbstractReporter class >> open [
	^ self new open
]

{ #category : #operations }
AbstractReporter >> add: category method: aSymbol [
	^self categoryList add: (self categories add: category -> aSymbol) key
]

{ #category : #display }
AbstractReporter >> addAll: aWindow [

	aWindow
		addMorph: self buildInformationList
		fullFrame: self categoriesFrame.
		
	aWindow
		addMorph: self buildInformationText
		fullFrame: self reportFrame
]

{ #category : #'items creation' }
AbstractReporter >> buildInformationList [

	^ PluggableListMorph
		on: self
		list: #categoryList
		primarySelection: #categorySelected
		changePrimarySelection: #categorySelected:
		listSelection: #categoryAt:
		changeListSelection: #categoryAt:put:
		menu: nil
		keystroke: nil
]

{ #category : #'items creation' }
AbstractReporter >> buildInformationText [

	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		beForPlainText;
		beReadOnly;
		beWrapped;
		yourself.
	^ scrolledText
]

{ #category : #accessing }
AbstractReporter >> categories [
	^ categories ifNil: [ categories := IdentityDictionary new ]
]

{ #category : #display }
AbstractReporter >> categoriesFrame [
	^ LayoutFrame new rightFraction: 0.25
		
]

{ #category : #'accessing-categories' }
AbstractReporter >> categoryAt: anIndex [
	^ categoriesSelected includes: (self categoryList at: anIndex ifAbsent: [ ^ false ])
]

{ #category : #'accessing-categories' }
AbstractReporter >> categoryAt: anInteger put: aBoolean [
	categoriesSelected := categoriesSelected
		perform:
			(aBoolean
				ifTrue: [ #copyWith: ]
				ifFalse: [ #copyWithout: ])
		with: (self categoryList at: anInteger ifAbsent: [ ^ self ]).
	self updateReport
]

{ #category : #accessing }
AbstractReporter >> categoryList [
	^ categoryList ifNil: [ categoryList := OrderedCollection new ]
]

{ #category : #'accessing-categories' }
AbstractReporter >> categorySelected [
	^ 0
]

{ #category : #'accessing-categories' }
AbstractReporter >> categorySelected: anInteger [

	self changed: #categorySelected
]

{ #category : #private }
AbstractReporter >> enumerate: aBlock on: aStream [
	self enumerate: aBlock startAt: 0 on: aStream
]

{ #category : #private }
AbstractReporter >> enumerate: aBlock startAt: first on: aStream [
	"Utilitymethod to enumerate Options or Parameters from first to 1000"

	| idx value |
	idx := first.
	[ 
	value := aBlock value: idx.
	value isNil or: [ idx > 1000 ] ]
		whileFalse: [ 
			aStream
				nextPut: $#;
				nextPutAll: idx printString;
				tab;
				nextPutAll: value;
				cr.
			idx := idx + 1 ].
	idx = first
		ifTrue: [ 
			aStream
				nextPutAll: 'none';
				cr ]
]

{ #category : #'accessing-ui' }
AbstractReporter >> extent [
	^ 640 @ 480
]

{ #category : #'printing-report' }
AbstractReporter >> header: aString on: aStream [
	aStream
		withAttribute: TextEmphasis bold
		do: [ aStream
				nextPutAll: aString;
				cr.
			aString size timesRepeat: [ aStream nextPut: $- ].
			aStream cr ]
]

{ #category : #'accessing-ui' }
AbstractReporter >> initialExtent [
	^ 640 @ 480
]

{ #category : #display }
AbstractReporter >> open [
	| window |
	window := StandardWindow new model: self.		
	self addAll: window.
	window openInWorld.
	window title: self label.
	^ window			
]

{ #category : #operations }
AbstractReporter >> refresh [
	self updateReport
]

{ #category : #display }
AbstractReporter >> reportFrame [
	^ LayoutFrame new leftFraction: 0.25


]

{ #category : #accessing }
AbstractReporter >> sourceTextModel [
	^ sourceTextModel
		ifNil: [ sourceTextModel := RubScrolledTextModel new interactionModel: self ]
]

{ #category : #updating }
AbstractReporter >> updateReport [
	self sourceTextModel setText: (Text streamContents: [:stream | 
						self categoryList do: [:each |
							(categoriesSelected includes: each) ifTrue: [
								self perform: (categories at: each) with: stream.
								stream cr]]])

]
