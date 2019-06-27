"
A label
"
Class {
	#name : #SpToolBarLabel,
	#superclass : #SpToolBarItemPresenter,
	#instVars : [
		'contents'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarLabel class >> adapterName [

	^ #ToolBarLabelAdapter
]

{ #category : #accessing }
SpToolBarLabel >> contents [
	^ contents
]

{ #category : #accessing }
SpToolBarLabel >> contents: anObject [
	contents := anObject
]
