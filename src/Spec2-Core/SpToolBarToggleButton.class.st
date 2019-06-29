"
A toggle button which is either selected or unselected
"
Class {
	#name : #SpToolBarToggleButton,
	#superclass : #SpToolBarButton,
	#instVars : [
		'selected'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarToggleButton class >> adapterName [

	^ #ToolBarToggleButtonAdapter
]

{ #category : #selection }
SpToolBarToggleButton >> beSelected [
	
	self setSelection: true
]

{ #category : #selection }
SpToolBarToggleButton >> beUnselected [
	
	self setSelection: false
]

{ #category : #execution }
SpToolBarToggleButton >> execute: state [

	self setSelection: state.
	self action cull: state cull: self
]

{ #category : #initialization }
SpToolBarToggleButton >> initialize [

	super initialize.
	selected := false
]

{ #category : #execution }
SpToolBarToggleButton >> isSelected [

	^ selected
]

{ #category : #selection }
SpToolBarToggleButton >> setSelection: aBoolean [

	selected := aBoolean.
	self changed: #isSelected
]
