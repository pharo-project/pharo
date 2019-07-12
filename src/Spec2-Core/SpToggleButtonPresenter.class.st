Class {
	#name : #SpToggleButtonPresenter,
	#superclass : #SpAbstractFormButtonPresenter,
	#instVars : [
		'#action => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpToggleButtonPresenter class >> adapterName [
	
	^ #ToggleButtonAdapter
]

{ #category : #specs }
SpToggleButtonPresenter class >> title [

	^ 'Toggle Button'
]

{ #category : #initialization }
SpToggleButtonPresenter >> action [ 
	
	^ action
]

{ #category : #initialization }
SpToggleButtonPresenter >> action: aBlock [
	
	^ action := aBlock
]

{ #category : #initialization }
SpToggleButtonPresenter >> initialize [

	super initialize.
	action := [  ]
]
