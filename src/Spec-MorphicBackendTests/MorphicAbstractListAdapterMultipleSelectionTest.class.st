Class {
	#name : #MorphicAbstractListAdapterMultipleSelectionTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #testing }
MorphicAbstractListAdapterMultipleSelectionTest class >> isAbstract [
	^ self == MorphicAbstractListAdapterMultipleSelectionTest
]

{ #category : #running }
MorphicAbstractListAdapterMultipleSelectionTest >> adapter [

	^ presenter adapter
]

{ #category : #running }
MorphicAbstractListAdapterMultipleSelectionTest >> setUp [

	super setUp.

	window := presenter
		beMultipleSelection;
		items: #(10 20 30);
		openWithSpec.
]

{ #category : #'tests-model-to-widget' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSelectManyIndexesKeepsAllSelected [
	presenter selectIndex: 1.
	presenter selectIndex: 2.
	self
		assert: self widget selectedIndexes asArray
		equals: #(1 2)
]

{ #category : #'tests-model-to-widget' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSelectPresenterIndexOutsideRangeUnsetsSelectedIndexInWidget [
	presenter selectIndex: 100.
	self assert: self widget selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSelectPresenterIndexSetsSelectedIndexInWidget [
	presenter selectIndex: 1.
	self assert: (self widget selectedIndexes includes: 1)
]

{ #category : #'tests-model-to-widget' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSelectPresenterItemSetsSelectedIndexInWidget [
	presenter selectItem: 20.
	self assert: (self widget selectedIndexes includes: 2)
]

{ #category : #'tests-widget-to-model' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self widget selectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndexes ].
	self widget selectIndex: 1.
	self assert: (selectedIndex includes: 1)
]

{ #category : #'tests-widget-to-model' }
MorphicAbstractListAdapterMultipleSelectionTest >> testSubscribeToEventDoesNotRaiseEvent [

	presenter selection
		whenChangedDo: [ :selection | self fail ].
]

{ #category : #'tests-widget-to-model' }
MorphicAbstractListAdapterMultipleSelectionTest >> testUnselectAllInWidgetRaisesEmptySelectionChangeEvent [
	| gotSelection |
	self widget selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | gotSelection := selection ].
	self widget selectIndex: 0.
	self assert: gotSelection isEmpty
]

{ #category : #'tests-widget-to-model' }
MorphicAbstractListAdapterMultipleSelectionTest >> testUnselectAllInWidgetRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	self widget selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self widget selectIndex: 0.
	self assert: counter equals: 1
]

{ #category : #running }
MorphicAbstractListAdapterMultipleSelectionTest >> widget [

	^ self adapter widget
]
