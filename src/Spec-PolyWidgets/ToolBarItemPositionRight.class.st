"
This class has a single instance representing a right item position.

Do not use the class directly, instead use:

	ITItemPosition right
"
Class {
	#name : #ToolBarItemPositionRight,
	#superclass : #ToolBarItemPosition,
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #accessing }
ToolBarItemPositionRight >> addItem: anObject into: aToolbar [
	aToolbar addItemRight: anObject
]
