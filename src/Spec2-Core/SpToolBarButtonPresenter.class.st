"
A button with an icon and label associated with an action and a badge 
"
Class {
	#name : #SpToolBarButtonPresenter,
	#superclass : #SpToolBarItemPresenter,
	#instVars : [
		'#label => SpObservableSlot',
		'#icon => SpObservableSlot',
		'#action',
		'#badge'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarButtonPresenter class >> adapterName [

	^ #ToolBarButtonAdapter
]

{ #category : #accessing }
SpToolBarButtonPresenter >> action [
	^ action
]

{ #category : #accessing }
SpToolBarButtonPresenter >> action: anObject [
	action := anObject
]

{ #category : #accessing }
SpToolBarButtonPresenter >> badge [
	^ badge
]

{ #category : #accessing }
SpToolBarButtonPresenter >> badge: aString [
	badge := aString
]

{ #category : #simulating }
SpToolBarButtonPresenter >> click [
	
	self execute
]

{ #category : #execution }
SpToolBarButtonPresenter >> execute [
	self action cull: self
]

{ #category : #accessing }
SpToolBarButtonPresenter >> icon [
	^ icon
]

{ #category : #accessing }
SpToolBarButtonPresenter >> icon: anObject [
	icon := anObject
]

{ #category : #initialization }
SpToolBarButtonPresenter >> initialize [
	super initialize.
]

{ #category : #accessing }
SpToolBarButtonPresenter >> label [
	^ label
]

{ #category : #accessing }
SpToolBarButtonPresenter >> label: anObject [
	label := anObject
]

{ #category : #'api-events' }
SpToolBarButtonPresenter >> whenIconChangedDo: aBlock [ 

	self property: #icon whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpToolBarButtonPresenter >> whenLabelChangedDo: aBlock [ 

	self property: #label whenChangedDo: aBlock 
]
