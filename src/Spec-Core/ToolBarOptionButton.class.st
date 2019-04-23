"
An option button
"
Class {
	#name : #ToolBarOptionButton,
	#superclass : #ToolBarToggleButton,
	#instVars : [
		'buttonGroup'
	],
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #specs }
ToolBarOptionButton class >> adapterName [

	^ #ToolBarOptionAdapter
]

{ #category : #accessing }
ToolBarOptionButton >> buttonGroup: anITOptionButtonGroup [ 
	
	buttonGroup := anITOptionButtonGroup
]

{ #category : #execution }
ToolBarOptionButton >> execute: state [

	buttonGroup unselectAll.
	self beSelected.
	self action cull: state cull: self
]
