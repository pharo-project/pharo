"
I'm a datasource to interpret messages in teh form provided for a PluggableIconListMorph.
"
Class {
	#name : #FTPluggableIconListMorphAdaptorDataSource,
	#superclass : #FTDataSource,
	#instVars : [
		'model',
		'getListSelector',
		'getListElementSelector',
		'wrapSelector',
		'getIconSelector',
		'getListSizeSelector',
		'getMenuSelector',
		'dropItemSelector',
		'wantsDropSelector'
	],
	#category : #'Morphic-Widgets-FastTable-PluggableAdaptor'
}

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> cellColumn: column row: rowIndex [
	| object text icon |
	object := self elementAt: rowIndex.
	
	icon := self getIconSelector ifNotNil: [ :selector | 
		self model perform: selector with: object ].
	text := column transform: (self elementWrapped: object).
	
	^ self createCellText: text icon: icon.
]

{ #category : #private }
FTPluggableIconListMorphAdaptorDataSource >> createCellText: aString icon: anIcon [ 
	| text icon cell |
	text := aString.
	icon := anIcon.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.
	icon
		ifNotNil:
			[ icon := icon asMorph.
			icon class = ImageMorph
				ifTrue: [ icon form: (icon form scaledToSize: 16 @ 16) ]
				ifFalse: [ icon bounds: (0 @ 0 corner: 16 @ 16) ].
			cell addMorphBack: icon ].
	cell addMorphBack: text asMorph.
	^ cell
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> dragTransferType [
	^ self getListElementSelector
]

{ #category : #'drag and drop' }
FTPluggableIconListMorphAdaptorDataSource >> dropElements: aCollection index: rowIndex [
	self dropItemSelector ifNil: [ ^ self ].
	self model 
		perform: self dropItemSelector 
		with: aCollection
		with: rowIndex
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> dropItemSelector [
	^ dropItemSelector 
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> dropItemSelector: aSelector [
	dropItemSelector := aSelector

]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> elementAt: rowIndex [
	^ self getListElementSelector 
		ifNotNil: [ 
			self model perform: self getListElementSelector with: rowIndex ]
		ifNil: [ 
			(self model perform: self getListSelector) at: rowIndex  ]
]

{ #category : #private }
FTPluggableIconListMorphAdaptorDataSource >> elementWrapped: object [
	^ self wrapSelector 
		ifNotNil: [ :selector | self model perform: selector with: object ] 
		ifNil: [ object ]
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> elements [
	^self model getList
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getIconSelector [
	^ getIconSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getIconSelector: anObject [
	getIconSelector := anObject
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getListElementSelector [
	^ getListElementSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getListElementSelector: anObject [
	getListElementSelector := anObject
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getListSelector [
	^ getListSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getListSelector: anObject [
	getListSelector := anObject
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getListSizeSelector [
	^ getListSizeSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getListSizeSelector: anObject [
	getListSizeSelector := anObject
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getMenuSelector [
	^ getMenuSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> getMenuSelector: anObject [
	getMenuSelector := anObject
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> menuColumn: column row: rowIndex [
	| menu |
	
	self getMenuSelector ifNil: [ ^ nil ].
	
	menu := UIManager default newMenuIn: self table for: self model.
	self getMenuSelector numArgs = 1 ifTrue: [ 
		^ self model perform: self getMenuSelector with: menu ].
	^ self model 
		perform: self getMenuSelector
		with: menu
		with: false 

]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> model [
	^ model
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> model: anObject [
	model := anObject
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> newDataSourceMatching: aFTFilter [
	| newElements wrappedItem text newDataSource modelCopy |
	newElements := self elements select: [ :each | 
		wrappedItem := self elementWrapped: each.
		table columns anySatisfy: [ :column | 
			text := column transform: wrappedItem.
			aFTFilter matches: text]].
	
	newDataSource := self copy.
	modelCopy := self model copy.
	modelCopy prepareForFilteredDataSourceWith: newElements.
	newDataSource model: modelCopy.
	
	^newDataSource
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> numberOfRows [
	^ self model 
		ifNotNil: [ 
			self getListSizeSelector 
				ifNotNil: [ self model perform: self getListSizeSelector ]
				ifNil: [ (self model perform: self getListSelector) size ] ]
		ifNil: [ 0 ]
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptorDataSource >> searchText: aString [
	| search text result |
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	result := OrderedCollection new.
	search := aString trimBoth asLowercase.
	1 to: self numberOfRows do: [ :rowIndex | 
		text := (self elementWrapped: (self elementAt: rowIndex)) contents trimBoth asLowercase.
		(text beginsWith: search)
			ifTrue: [ result add: rowIndex ] ].
	^ result asArray
]

{ #category : #'drag and drop' }
FTPluggableIconListMorphAdaptorDataSource >> transferFor: passenger from: aMorph [
    ^(self model transferFor: passenger from: self table) buildWithSpec
]

{ #category : #'drag and drop' }
FTPluggableIconListMorphAdaptorDataSource >> wantsDropElements: aCollection type: type index: rowIndex [
	self dropItemSelector ifNil: [ ^ false ].
	self wantsDropSelector ifNil: [ ^ true ].
	^ self model 
		perform: self wantsDropSelector 
		with: aCollection
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> wantsDropSelector [
	^ wantsDropSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> wantsDropSelector: aSelector [
	wantsDropSelector := aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> wrapSelector [
	^ wrapSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptorDataSource >> wrapSelector: anObject [
	wrapSelector := anObject
]
