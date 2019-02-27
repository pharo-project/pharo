"
Abstract superclass for toolbar visual items like buttons, labels or other
"
Class {
	#name : #ToolBarItemPresenter,
	#superclass : #AbstractWidgetPresenter,
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #factory }
ToolBarItemPresenter >> buildMorphOnToolbar: aToolbar [

	self subclassResponsibility
]
