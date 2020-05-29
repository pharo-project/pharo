"
An option button
"
Class {
	#name : #SpToolBarOptionButtonPresenter,
	#superclass : #SpToolBarToggleButtonPresenter,
	#instVars : [
		'buttonGroup'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarOptionButtonPresenter class >> adapterName [

	^ #ToolBarOptionAdapter
]

{ #category : #accessing }
SpToolBarOptionButtonPresenter >> buttonGroup: anITOptionButtonGroup [ 
	
	buttonGroup := anITOptionButtonGroup
]

{ #category : #execution }
SpToolBarOptionButtonPresenter >> execute: state [

	buttonGroup unselectAll.
	self beSelected.
	self action cull: state cull: self
]
