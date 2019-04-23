Class {
	#name : #ToggleButtonPresenter,
	#superclass : #AbstractFormButtonPresenter,
	#instVars : [
		'#action => SpecObservableSlot'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
ToggleButtonPresenter class >> adapterName [
	
	^ #ToggleButtonAdapter
]

{ #category : #specs }
ToggleButtonPresenter class >> title [

	^ 'Toggle Button'
]

{ #category : #initialization }
ToggleButtonPresenter >> action [ 
	
	^ action
]

{ #category : #initialization }
ToggleButtonPresenter >> action: aBlock [
	
	^ action := aBlock
]

{ #category : #initialization }
ToggleButtonPresenter >> initialize [
	self class initializeSlots: self.
	super initialize.
	
	action := [  ]
]
