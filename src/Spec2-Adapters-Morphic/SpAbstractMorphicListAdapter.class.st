"
I am an abstract class with common behaviour for list and table adapters.
See my subclasses for more details.
"
Class {
	#name : #SpAbstractMorphicListAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-List'
}

{ #category : #testing }
SpAbstractMorphicListAdapter class >> isAbstract [
	^ self = SpAbstractMorphicListAdapter
]

{ #category : #initialization }
SpAbstractMorphicListAdapter >> addKeyBindingsTo: aWidget [

	super addKeyBindingsTo: aWidget.
	self addContextMenuKeyBindings: aWidget
]

{ #category : #scrolling }
SpAbstractMorphicListAdapter >> configureScrolling [
	self presenter verticalAlignment
		whenChangedDo: [ widget
				scrollToIndex: self presenter verticalAlignment desiredVisibleRow ].
			
	self presenter
		whenDisplayDo: [ widget
				scrollToIndex: self presenter verticalAlignment desiredVisibleRow.
			self scrollingChanged ].
		
	widget
		onAnnouncement: FTScrollingChanged
		send: #scrollingChanged
		to: self.
]

{ #category : #'widget API' }
SpAbstractMorphicListAdapter >> elementAt: index [

	^ self widget dataSource elementAt: index
]

{ #category : #'widget API' }
SpAbstractMorphicListAdapter >> itemFilter [
	^ self model itemFilter
]

{ #category : #scrolling }
SpAbstractMorphicListAdapter >> scrollingChanged [
	widget container updateAllRows.
	self presenter verticalAlignment
		firstVisibleRowIndex: widget firstVisibleRowIndex;
		lastVisibleRowIndex: widget lastVisibleRowIndex
]

{ #category : #emulating }
SpAbstractMorphicListAdapter >> sendClickEventToRow: aNumber [
	| evt rowWidget |

	rowWidget := widget container submorphs at: aNumber.
	evt := MouseButtonEvent new 
		setType: nil 
		position: rowWidget bounds center 
		which: MouseButtonEvent redButton 
		buttons: MouseButtonEvent redButton 
		hand: self currentHand 
		stamp: nil.
	
	(widget handlesMouseDown: evt) ifTrue: [
		widget handleMouseDown: evt.
		widget handleMouseUp: evt ]
]

{ #category : #emulating }
SpAbstractMorphicListAdapter >> sendClickWithShiftEventToRow: aNumber [
	| evt rowWidget |

	rowWidget := widget container submorphs at: aNumber.
	evt := MouseButtonEvent new 
		setType: nil 
		position: rowWidget bounds center 
		which: MouseButtonEvent redButton 
		buttons: (MouseButtonEvent redButton | 8 "shift = 8 mask")
		hand: self currentHand 
		stamp: nil.
	
	(widget handlesMouseDown: evt) ifTrue: [
		widget handleMouseDown: evt.
		widget handleMouseUp: evt ]
]

{ #category : #private }
SpAbstractMorphicListAdapter >> showContextMenu [

	self widgetDo: [ :w |
		w showMenuForIndex: { self presenter selection selectedIndex. 1 } ]
]

{ #category : #factory }
SpAbstractMorphicListAdapter >> updateItemFilterBlockWith: block [

	^ block
		ifNotNil: [ 
			widget
				enableFilter: (SpFTSpecFilter block: block);
				explicitFunction ]
		ifNil: [ 
			self updateSearch ]
]

{ #category : #factory }
SpAbstractMorphicListAdapter >> updateSearch [

	self presenter isSearchEnabled
		ifTrue: [ widget enableSearch ]
		ifFalse: [ widget disableFunction ]
]
