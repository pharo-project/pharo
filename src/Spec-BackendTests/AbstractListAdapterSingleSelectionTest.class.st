Class {
	#name : #AbstractListAdapterSingleSelectionTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractListAdapterSingleSelectionTest class >> isAbstract [
	^ self == AbstractListAdapterSingleSelectionTest
]

{ #category : #running }
AbstractListAdapterSingleSelectionTest >> initializeTestedInstance [

	super initializeTestedInstance.

	presenter
		beSingleSelection;
		items: #(10 20 30).

]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterSingleSelectionTest >> testSelectManyIndexesKeepsLastSelectionInWidget [
	presenter selectIndex: 1.
	presenter selectIndex: 2.
	self assert: self widget selectedIndexes equals: #(2)
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterSingleSelectionTest >> testSelectPresenterIndexOutsideRangeUnsetsSelectedIndexInWidget [
	presenter selectIndex: 100.
	self assert: self widget selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterSingleSelectionTest >> testSelectPresenterIndexSetsSelectedIndexInWidget [
	presenter selectIndex: 1.
	self assert: (self widget selectedIndexes includes: 1)
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterSingleSelectionTest >> testSelectPresenterItemSetsSelectedIndexInWidget [
	presenter selectItem: 20.
	self assert: (self widget selectedIndexes includes: 2)
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	self widget selectIndex: 1.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testSubscribeToEventDoesNotRaiseEvent [

	presenter selection
		whenChangedDo: [ :selection | self fail ].
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testUnselectAllInWidgetNotRaisesEmptySelectionChangeEvent [
	| gotSelection |
	presenter selection
		whenChangedDo: [ :selection | gotSelection := selection ].
	self adapter selectIndex: 0.
	self assert: gotSelection isNil
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testUnselectAllInWidgetWithoutSelectionDoesNotRaiseEvent [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 0.
	self assert: counter equals: 0
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterSingleSelectionTest >> testUnselectPresenterIndexUnselectsIndexInWidget [
	presenter
		selectIndex: 1;
		unselectIndex: 1.
	self assert: self widget selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterSingleSelectionTest >> testUnselectPresenterItemUnselectsIndexInWidget [
	presenter
		selectItem: 20;
		unselectItem: 20.
	self assert: self widget selectedIndexes isEmpty
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testUnselectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	self adapter selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 0.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterSingleSelectionTest >> testUnselectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	self adapter selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	self adapter selectIndex: 0.
	self assert: selectedIndex equals: 0
]
