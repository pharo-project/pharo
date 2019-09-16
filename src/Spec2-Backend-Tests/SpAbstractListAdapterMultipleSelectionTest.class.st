Class {
	#name : #SpAbstractListAdapterMultipleSelectionTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractListAdapterMultipleSelectionTest class >> isAbstract [
	^ self == SpAbstractListAdapterMultipleSelectionTest
]

{ #category : #running }
SpAbstractListAdapterMultipleSelectionTest >> initializeTestedInstance [

	super initializeTestedInstance.

	presenter
		beMultipleSelection;
		items: {10. 20. 30. 'aaa' asText}
]

{ #category : #tests }
SpAbstractListAdapterMultipleSelectionTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterMultipleSelectionTest >> testSelectManyIndexesKeepsAllSelected [
	presenter selectIndex: 1.
	presenter selectIndex: 2.
	self
		assert: self adapter selectedIndexes asArray
		equals: #(1 2)
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterMultipleSelectionTest >> testSelectPresenterIndexOutsideRangeUnsetsSelectedIndexInWidget [
	presenter selectIndex: 100.
	self assert: self adapter selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterMultipleSelectionTest >> testSelectPresenterIndexSetsSelectedIndexInWidget [
	presenter selectIndex: 1.
	self assert: (self adapter selectedIndexes includes: 1)
]

{ #category : #'tests-model-to-widget' }
SpAbstractListAdapterMultipleSelectionTest >> testSelectPresenterItemSetsSelectedIndexInWidget [
	presenter selectItem: 20.
	self assert: (self adapter selectedIndexes includes: 2)
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterMultipleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterMultipleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndexes ].
	self adapter selectIndex: 1.
	self assert: (selectedIndex includes: 1)
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterMultipleSelectionTest >> testStringColumnsShouldBeNonEditable [

	self assert: (self adapter isNonEditableRow: 4 column: 1)
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterMultipleSelectionTest >> testSubscribeToEventDoesNotRaiseEvent [

	presenter selection
		whenChangedDo: [ :selection | self fail ].
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterMultipleSelectionTest >> testUnselectAllInWidgetRaisesEmptySelectionChangeEvent [
	| gotSelection |
	self adapter selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | gotSelection := selection ].
	self adapter selectIndex: 0.
	self assert: gotSelection isEmpty
]

{ #category : #'tests-widget-to-model' }
SpAbstractListAdapterMultipleSelectionTest >> testUnselectAllInWidgetRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	self adapter selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 0.
	self assert: counter equals: 1
]
