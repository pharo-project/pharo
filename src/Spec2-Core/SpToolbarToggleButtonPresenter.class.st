"
A presenter to display a toggle button in a `SpToolbarPresenter`.
A toggle button is a button that can be activated (toggled) or deactivated (untoggled).

"
Class {
	#name : #SpToolbarToggleButtonPresenter,
	#superclass : #SpToolbarButtonPresenter,
	#instVars : [
		'#selected => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarToggleButtonPresenter class >> adapterName [

	^ #ToolbarToggleButtonAdapter
]

{ #category : #documentation }
SpToolbarToggleButtonPresenter class >> documentFactoryMethodSelector [

	^ #newToolbarToggleButton
]

{ #category : #api }
SpToolbarToggleButtonPresenter >> beSelected [
	"Set the status of the button to 'selected' (toggled)"
	
	self setSelection: true
]

{ #category : #api }
SpToolbarToggleButtonPresenter >> beUnselected [
	"Set the status of the button to 'unselected' (untoggled)"

	self setSelection: false
]

{ #category : #execution }
SpToolbarToggleButtonPresenter >> execute: state [

	self setSelection: state.
	self action 
		cull: state 
		cull: self
]

{ #category : #initialization }
SpToolbarToggleButtonPresenter >> initialize [

	super initialize.
	selected := false
]

{ #category : #testing }
SpToolbarToggleButtonPresenter >> isSelected [

	^ selected
]

{ #category : #private }
SpToolbarToggleButtonPresenter >> setSelection: aBoolean [

	selected := aBoolean.
	self changed: #isSelected
]

{ #category : #api }
SpToolbarToggleButtonPresenter >> toggle [
	"Toggles the status of the button"
	
	self setSelection: selected not
]

{ #category : #private }
SpToolbarToggleButtonPresenter >> whenChangedDo: aBlock [
	"Only execute aBlock if old value is different from new value"

	self
		property: #selected
		whenChangedDo: [ :newSelectedValue :oldSelectedValue | 
			newSelectedValue = oldSelectedValue
				ifFalse: [ aBlock cull: newSelectedValue cull: oldSelectedValue ] ]
]

{ #category : #'api-events' }
SpToolbarToggleButtonPresenter >> whenSelectedDo: aBlock [
	"Inform when the button has been selected. 
	 `aBlock` receives zero arguments."

	self whenChangedDo: [ :newSelectedValue | 
		newSelectedValue ifTrue: [ aBlock value ] ]
]

{ #category : #'api-events' }
SpToolbarToggleButtonPresenter >> whenToggledDo: aBlock [ 
	"Inform when the button has been toggles. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #selected whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpToolbarToggleButtonPresenter >> whenUnselectedDo: aBlock [
	"Inform when the button has been unselected. 
	 `aBlock` receives zero arguments."

	self whenChangedDo: [ :newSelectedValue | 
		newSelectedValue ifFalse: [ aBlock value ] ]
]
