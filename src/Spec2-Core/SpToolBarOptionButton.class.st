"
An option button
"
Class {
	#name : #SpToolBarOptionButton,
	#superclass : #SpToolBarToggleButton,
	#instVars : [
		'buttonGroup'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarOptionButton class >> adapterName [

	^ #ToolBarOptionAdapter
]

{ #category : #accessing }
SpToolBarOptionButton >> buttonGroup: anITOptionButtonGroup [ 
	
	buttonGroup := anITOptionButtonGroup
]

{ #category : #execution }
SpToolBarOptionButton >> execute: state [

	buttonGroup unselectAll.
	self beSelected.
	self action cull: state cull: self
]
