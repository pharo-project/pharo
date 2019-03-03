"
Description
--------------------

I am a simple presenter to add spacers to spec layouts.
"
Class {
	#name : #SpacerPresenter,
	#superclass : #AbstractWidgetPresenter,
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
SpacerPresenter class >> adapterName [

	^ #SpacerAdapter
]
