"
A toggle button which is either selected or unselected
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

{ #category : #selection }
SpToolbarToggleButtonPresenter >> beSelected [
	
	self setSelection: true
]

{ #category : #selection }
SpToolbarToggleButtonPresenter >> beUnselected [
	
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

{ #category : #selection }
SpToolbarToggleButtonPresenter >> setSelection: aBoolean [

	selected := aBoolean.
	self changed: #isSelected
]

{ #category : #selection }
SpToolbarToggleButtonPresenter >> toggle [
	
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

{ #category : #'api - events' }
SpToolbarToggleButtonPresenter >> whenSelectedDo: aBlock [
	self whenChangedDo: [ :newSelectedValue | 
			newSelectedValue
				ifTrue: [ aBlock value ] ]
]

{ #category : #'api - events' }
SpToolbarToggleButtonPresenter >> whenToggledDo: aBlock [ 

	self property: #selected whenChangedDo: aBlock 
]

{ #category : #'api - events' }
SpToolbarToggleButtonPresenter >> whenUnselectedDo: aBlock [
	self whenChangedDo: [ :newSelectedValue | 
			newSelectedValue
				ifFalse: [ aBlock value ] ]
]
