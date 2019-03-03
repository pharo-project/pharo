"
A toggle button which is either selected or unselected
"
Class {
	#name : #ToolBarToggleButton,
	#superclass : #ToolBarButton,
	#instVars : [
		'selected'
	],
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #specs }
ToolBarToggleButton class >> adapterName [

	^ #ToolBarToggleButtonAdapter
]

{ #category : #selection }
ToolBarToggleButton >> beSelected [
	
	self setSelection: true
]

{ #category : #selection }
ToolBarToggleButton >> beUnselected [
	
	self setSelection: false
]

{ #category : #execution }
ToolBarToggleButton >> execute: state [

	self setSelection: state.
	self action cull: state cull: self
]

{ #category : #initialization }
ToolBarToggleButton >> initialize [

	super initialize.
	selected := false
]

{ #category : #execution }
ToolBarToggleButton >> isSelected [

	^ selected
]

{ #category : #selection }
ToolBarToggleButton >> setSelection: aBoolean [

	selected := aBoolean.
	self changed: #isSelected
]
