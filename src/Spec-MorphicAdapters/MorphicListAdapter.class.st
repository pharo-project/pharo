"
I am the adapter used to bridge a ListPresenter and a PluggableListMorph
"
Class {
	#name : #MorphicListAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-List'
}

{ #category : #'widget API' }
MorphicListAdapter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [
	| item index |
	
	index := source rowAtLocation: event position.
	item := self presenter model at: index ifAbsent: [ nil ].
	
	^ self acceptDropBlock valueWithEnoughArguments: { draggedMorph model model . event. source model model. item. index }
]

{ #category : #'spec protocol' }
MorphicListAdapter >> autoDeselect: aBoolean [

	self widgetDo: [ :w | w autoDeselect: aBoolean ]
]

{ #category : #'widget API' }
MorphicListAdapter >> backgroundColorFor: anItem [
	^ self model backgroundColorFor: anItem
]

{ #category : #'widget API' }
MorphicListAdapter >> backgroundColorFor: anItem at: index [

	^ self model backgroundColorFor: anItem at: index
]

{ #category : #factory }
MorphicListAdapter >> buildWidget [
	| datasource |
	datasource := SpecListFastTableDataSource new.
	datasource model: self model.
	widget := FTTableMorph new
		dataSource: datasource;
		hideColumnHeaders;
		beResizable;
		columns: {self newListColumn};
		setMultipleSelection: self model isMultipleSelection;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		onAnnouncement: FTSelectionChanged send: #selectionChanged: to: self;
		onAnnouncement: FTStrongSelectionChanged send: #strongSelectionChanged: to: self;
		yourself.
	self presenter whenModelChangedDo: [ widget refresh ].
	self presenter whenSelectionChangedDo: [ self refreshWidgetSelection ].
	self presenter selection whenChangedDo: [ self refreshWidgetSelection ].
	self refreshWidgetHeaderTitle.
	self refreshWidgetSelection.
	self presenter whenItemFilterBlockChangedDo: [ :block | self updateItemFilterBlockWith: block ].
	self updateItemFilterBlockWith: self itemFilterBlock.

	widget bindKeyCombination: Character space toAction: [ self model clickOnSelectedItem ].

	^ widget
]

{ #category : #emulating }
MorphicListAdapter >> hasFilter [
	^ self widget submorphs anySatisfy: [ :each | each isKindOf: RubTextFieldMorph	"This morph is the explicit filter of the list" ]
]

{ #category : #accessing }
MorphicListAdapter >> isShowColumnHeaders [
	
	^ widget isShowColumnHeaders 
]

{ #category : #'widget API' }
MorphicListAdapter >> itemFilterBlock [
	^ self model itemFilterBlock
]

{ #category : #'widget API' }
MorphicListAdapter >> listElementAt: anIndex ifAbsent: aBlock [

	^ self model listElementAt: anIndex ifAbsent: aBlock
]

{ #category : #'widget API' }
MorphicListAdapter >> listSize [

	^ self model listSize
]

{ #category : #factory }
MorphicListAdapter >> newListColumn [
	
	^ MorphicTableColumn new 
		model: (StringTableColumn
			title: self presenter headerTitle
			evaluated: self presenter displayBlock);
		yourself
]

{ #category : #'widget API' }
MorphicListAdapter >> refreshList [

	^ self widget refresh
]

{ #category : #'widget API' }
MorphicListAdapter >> refreshWidgetHeaderTitle [
	
	self presenter hasHeaderTitle 
		ifTrue: [ 
			self widget columns first model title: self presenter headerTitle.
			self widget
				showColumnHeaders;
				refresh. ]
		ifFalse: [ 
			self widget columns first model title: nil.
			self widget
				hideColumnHeaders;
				refresh ].
]

{ #category : #'widget API' }
MorphicListAdapter >> refreshWidgetList [
	(self widget showIndex < self widget dataSource numberOfRows
		and: [ self widget isIndexVisible: self widget showIndex ])
		ifFalse: [ self widget resetPosition ].
	self refreshWidgetSelection.
	(self widget hasSelection
		and: [ self widget isIndexVisible: self widget selectedIndex ])
		ifFalse: [ self widget resetPosition.
			self widget ensureVisibleFirstSelection ].
	self widget refresh
]

{ #category : #'widget API' }
MorphicListAdapter >> refreshWidgetSelection [
	| rowIndexes |
	rowIndexes := self presenter selection selectedIndexes.
	rowIndexes = self widget selectedIndexes
		ifTrue: [ ^ self ].
	rowIndexes isEmpty
		ifTrue: [ self widget basicSelectIndexes: #() ]
		ifFalse: [ self widget basicSelectIndexes: rowIndexes ].
	^ self widget refresh
]

{ #category : #emulating }
MorphicListAdapter >> rightClicked [
	| evt |

	evt := MouseButtonEvent new 
		setType: nil 
		position: widget center 
		which: MouseButtonEvent yellowButton 
		buttons: MouseButtonEvent yellowButton 
		hand: nil 
		stamp: nil.
	
	self widget click: evt
]

{ #category : #selecting }
MorphicListAdapter >> selectIndex: anInteger [ 
	
	(anInteger = 0 and: [ widget selectedIndexes isEmpty ]) ifTrue: [ ^ self ].	
	(widget selectedIndexes includes: anInteger) ifTrue: [^self].
	widget selectIndex: anInteger
]

{ #category : #factory }
MorphicListAdapter >> selectionChanged: ann [
	| diff |
	self presenter selection isMultipleSelection
		ifTrue: [ self presenter selection
				selectIndexes: (ann newSelectedIndexes reject: [ :e | e = 0 ]) ].
	self presenter selection isMultipleSelection
		ifFalse: [ self presenter selection
				selectIndex:
					(ann newSelectedIndexes
						ifNotEmpty: [ :indexes | indexes first ]
						ifEmpty: [ 0 ]) ].
	diff := ann newSelectedIndexes difference: ann oldSelectedIndexes.
	(diff notEmpty and: [ self presenter activatesOnSingleClick ])
		ifTrue: [ self presenter doActivateAtIndex: diff last ].
	ann newSelectedIndexes ifNotEmpty: [ ^ self ].
	self presenter selection unselectAll
]

{ #category : #events }
MorphicListAdapter >> strongSelectionChanged: aFTStrongSelectionChanged [
	self presenter activatesOnDoubleClick
		ifTrue: [ self presenter
				doubleClickAtIndex: aFTStrongSelectionChanged selectedIndex ]
]

{ #category : #factory }
MorphicListAdapter >> updateItemFilterBlockWith: block [
	^ block
		ifNotNil: [ widget
				enableFilter: (FTSpecFilter block: block);
				explicitFunction ]
		ifNil: [ widget disableFunction ]
]

{ #category : #events }
MorphicListAdapter >> updateMenu [
	
	"there is no need to updateMenu here because it will be resolved at the dataSource"
]
