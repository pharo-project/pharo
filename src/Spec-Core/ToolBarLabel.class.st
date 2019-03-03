"
A label
"
Class {
	#name : #ToolBarLabel,
	#superclass : #ToolBarItemPresenter,
	#instVars : [
		'help',
		'contents'
	],
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #specs }
ToolBarLabel class >> adapterName [

	^ #ToolBarLabelAdapter
]

{ #category : #factory }
ToolBarLabel >> buildMorphOnToolbar: aToolbar [

	^ (LabelMorph contents: contents)
		setBalloonText: help;
		yourself
]

{ #category : #accessing }
ToolBarLabel >> contents [
	^ contents
]

{ #category : #accessing }
ToolBarLabel >> contents: anObject [
	contents := anObject
]

{ #category : #accessing }
ToolBarLabel >> help [
	^ help
]

{ #category : #accessing }
ToolBarLabel >> help: anObject [
	help := anObject
]
