Class {
	#name : #SpTreeAdapterSingleSelectionTest,
	#superclass : #SpAbstractListAdapterSingleSelectionTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpTreeAdapterSingleSelectionTest >> classToTest [

	^ SpTreePresenter
]

{ #category : #running }
SpTreeAdapterSingleSelectionTest >> initializeTestedInstance [ 

	presenter
		beSingleSelection;
		roots: #(10 20 30).
	
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectManyIndexesKeepsLastSelectionInWidget [
	
	presenter selectPath: #(1).
	presenter selectPath: #(2).
	self assert: self adapter selectedPaths equals: #(#(2))
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectPresenterIndexOutsideRangeUnsetsSelectedIndexInWidget [
	
	presenter selectPath: #(100).
	self assert: self adapter selectedPaths isEmpty
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectPresenterIndexSetsSelectedIndexInWidget [
	
	presenter selectPath: #(1).
	self assert: (self adapter selectedPaths includes: #(1))
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectPresenterItemSetsSelectedIndexInWidget [
	
	self skip. "No selectItem for the moment"
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectPresenterWithScrollingMakeSelectedItemVisible [
	| visibleItems |

	presenter
		roots: (1 to: 200);
		openWithSpec.
		
	presenter selectPath: { 150 } scrollToSelection: true.
	visibleItems := presenter verticalAlignment firstVisibleRowIndex to: presenter verticalAlignment lastVisibleRowIndex.
		
	self assert: (visibleItems includes: presenter selection selectedPath first)
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectPresenterWithoutScrollingDoesNotScroll [
	| visibleItems verticalAlignment |
	
	presenter 
		roots: (1 to: 200);
		openWithSpec.
	verticalAlignment := presenter verticalAlignment.		
	visibleItems := verticalAlignment firstVisibleRowIndex to: verticalAlignment lastVisibleRowIndex.
	
	presenter selectPath: { 150 } scrollToSelection: false.
		
	self 
		assert: (verticalAlignment firstVisibleRowIndex to: verticalAlignment lastVisibleRowIndex) 
		equals: visibleItems
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	
	counter := 0.
	presenter selection whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectPath: #(1).
	self assert: counter equals: 1
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testSelectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedPath |
	
	presenter selection
		whenChangedDo: [ :selection | selectedPath := selection ].
	self adapter selectPath: #(1).
	self assert: selectedPath equals: #(1)
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectAllInWidgetNotRaisesEmptySelectionChangeEvent [
	| gotSelection |
	
	presenter selection
		whenChangedDo: [ :selection | gotSelection := selection ].
	self adapter selectPath: #().
	self assert: gotSelection isNil
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectAllInWidgetRaisesSelectionIndexChangeEventOnce [
	| counter |
	
	counter := 0.
	self adapter selectPath: #(1).
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectPath: #().
	self assert: counter equals: 1
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectAllInWidgetWithoutSelectionDoesNotRaiseEvent [
	| counter |
	
	counter := 0.
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectPath: #().
	self assert: counter equals: 0
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectPresenterIndexUnselectsIndexInWidget [
	
	presenter
		selectPath: #(1);
		unselectPath: #(1).
	self assert: self adapter selectedPaths isEmpty
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectPresenterItemUnselectsIndexInWidget [
	
	self skip. "No selectItem for the moment"
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectWidgetIndexRaisesSelectionIndexChangeEventOnce [
	| counter |
	
	counter := 0.
	self adapter selectPath: #(1).
	presenter selection
		whenChangedDo: [ :selection | counter := counter + 1 ].
	self adapter selectPath: #().
	self assert: counter equals: 1
]

{ #category : #'tests-model-to-widget' }
SpTreeAdapterSingleSelectionTest >> testUnselectWidgetIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedPath |
	
	self adapter selectPath: #(1).
	presenter selection 
		whenChangedDo: [ :selection | selectedPath := selection ].
	self adapter selectPath: #().
	self assertEmpty: selectedPath
]
