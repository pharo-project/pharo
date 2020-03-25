Class {
	#name : #SpToggleButtonPresenter,
	#superclass : #SpAbstractFormButtonPresenter,
	#instVars : [
		'#action => SpObservableSlot',
		'#icon => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpToggleButtonPresenter class >> adapterName [
	
	^ #ToggleButtonAdapter
]

{ #category : #example }
SpToggleButtonPresenter class >> example [

	self new
		icon: (self iconNamed: #smallOk);
		whenActivatedDo: [ UIManager default defer: [ self inform: 'Activated' ] ];
		whenDeactivatedDo: [ UIManager default defer: [ self inform: 'Deactivated' ] ];
		openWithSpec
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
	
	action := aBlock
]

{ #category : #accessing }
SpToggleButtonPresenter >> icon [

	^ icon
]

{ #category : #accessing }
SpToggleButtonPresenter >> icon: anIcon [

	icon := anIcon
]

{ #category : #initialization }
SpToggleButtonPresenter >> initialize [

	super initialize.
	action := [ ]
]

{ #category : #'api-events' }
SpToggleButtonPresenter >> whenIconChangedDo: aBlock [

	self property: #icon whenChangedDo: aBlock
]
