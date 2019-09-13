"
This class has a single instance representing a left item position.

Do not use the class directly, instead use:

	SpToolBarItemPosition left
"
Class {
	#name : #SpToolBarItemPositionLeft,
	#superclass : #SpToolBarItemPosition,
	#category : #'Spec2-PolyWidgets-ToolBar'
}

{ #category : #accessing }
SpToolBarItemPositionLeft >> addItem: anObject into: aToolbar [
	aToolbar addItemLeft: anObject
]
