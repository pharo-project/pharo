Class {
	#name : #AbstractListAdapterMultipleSelectionTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractListAdapterMultipleSelectionTest class >> isAbstract [
	^ self == AbstractListAdapterMultipleSelectionTest
]

{ #category : #running }
AbstractListAdapterMultipleSelectionTest >> initializeTestedInstance [

	super initializeTestedInstance.

	presenter
		beMultipleSelection;
		items: #(10 20 30)
]

{ #category : #tests }
AbstractListAdapterMultipleSelectionTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpecMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterMultipleSelectionTest >> testSelectManyIndexesKeepsAllSelected [
	presenter selectIndex: 1.
	presenter selectIndex: 2.
	self
		assert: self widget selectedIndexes asArray
		equals: #(1 2)
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterMultipleSelectionTest >> testSelectPresenterIndexOutsideRangeUnsetsSelectedIndexInWidget [
	presenter selectIndex: 100.
	self assert: self widget selectedIndexes isEmpty
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterMultipleSelectionTest >> testSelectPresenterIndexSetsSelectedIndexInWidget [
	presenter selectIndex: 1.
	self assert: (self widget selectedIndexes includes: 1)
]

{ #category : #'tests-model-to-widget' }
AbstractListAdapterMultipleSelectionTest >> testSelectPresenterItemSetsSelectedIndexInWidget [
	presenter selectItem: 20.
	self assert: (self widget selectedIndexes includes: 2)
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterMultipleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self widget selectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterMultipleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndexes ].
	self widget selectIndex: 1.
	self assert: (selectedIndex includes: 1)
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterMultipleSelectionTest >> testSubscribeToEventDoesNotRaiseEvent [

	presenter selection
		whenChangedDo: [ :selection | self fail ].
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterMultipleSelectionTest >> testUnselectAllInWidgetRaisesEmptySelectionChangeEvent [
	| gotSelection |
	self widget selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | gotSelection := selection ].
	self adapter selectIndex: 0.
	self assert: gotSelection isEmpty
]

{ #category : #'tests-widget-to-model' }
AbstractListAdapterMultipleSelectionTest >> testUnselectAllInWidgetRaisesSelectionIndexChangeEventOnce [
	| counter |
	counter := 0.
	self widget selectIndex: 1.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectIndex: 0.
	self assert: counter equals: 1
]
