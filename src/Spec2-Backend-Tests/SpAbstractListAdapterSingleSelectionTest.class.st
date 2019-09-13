Class {
	#name : #SpAbstractListAdapterSingleSelectionTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractListAdapterSingleSelectionTest class >> isAbstract [
	^ self == SpAbstractListAdapterSingleSelectionTest
]

{ #category : #running }
SpAbstractListAdapterSingleSelectionTest >> initializeTestedInstance [

	super initializeTestedInstance.

	presenter
		beSingleSelection;
		items: #(10 20 30).

]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterSingleSelectionTest >> testSelectManyIndexesKeepsLastSelectionInWidget [
	presenter selectIndex: 1.
	presenter selectIndex: 2.
	self assert: self adapter selectedIndexes equals: #(2)
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterSingleSelectionTest >> testSelectPresenterIndexOutsideRangeUnsetsSelectedIndexInWidget [
	presenter selectIndex: 100.
	self assert: self adapter selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterSingleSelectionTest >> testSelectPresenterIndexSetsSelectedIndexInWidget [
	presenter selectIndex: 1.
	self assert: (self adapter selectedIndexes includes: 1)
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterSingleSelectionTest >> testSelectPresenterItemSetsSelectedIndexInWidget [
	presenter selectItem: 20.
	self assert: (self adapter selectedIndexes includes: 2)
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	self adapter selectIndex: 1.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testSubscribeToEventDoesNotRaiseEvent [

	presenter selection
		whenChangedDo: [ :selection | self fail ].
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testUnselectAllInWidgetNotRaisesEmptySelectionChangeEvent [
	| gotSelection |
	presenter selection
		whenChangedDo: [ :selection | gotSelection := selection ].
	self adapter selectIndex: 0.
	self assert: gotSelection isNil
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testUnselectAllInWidgetWithoutSelectionDoesNotRaiseEvent [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 0.
	self assert: counter equals: 0
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterSingleSelectionTest >> testUnselectPresenterIndexUnselectsIndexInWidget [
	presenter
		selectIndex: 1;
		unselectIndex: 1.
	self assert: self adapter selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterSingleSelectionTest >> testUnselectPresenterItemUnselectsIndexInWidget [
	presenter
		selectItem: 20;
		unselectItem: 20.
	self assert: self adapter selectedIndexes isEmpty
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testUnselectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	self adapter selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 0.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterSingleSelectionTest >> testUnselectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	self adapter selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	self adapter selectIndex: 0.
	self assert: selectedIndex equals: 0
]
