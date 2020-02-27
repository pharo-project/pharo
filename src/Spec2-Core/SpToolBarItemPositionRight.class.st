"
This class has a single instance representing a right item position.

Do not use the class directly, instead use:

	ITItemPosition right
"
Class {
	#name : #SpToolBarItemPositionRight,
	#superclass : #SpToolBarItemPosition,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #accessing }
SpToolBarItemPositionRight >> addItem: anObject into: aToolbar [
	aToolbar addItemRight: anObject
]
