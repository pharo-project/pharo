"
A label
"
Class {
	#name : #SpToolbarLabel,
	#superclass : #SpToolbarItemPresenter,
	#instVars : [
		'contents'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarLabel class >> adapterName [

	^ #ToolbarLabelAdapter
]

{ #category : #accessing }
SpToolbarLabel >> contents [
	^ contents
]

{ #category : #accessing }
SpToolbarLabel >> contents: anObject [
	contents := anObject
]
