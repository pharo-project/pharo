"
A button with an icon and label associated with an action and a badge 
"
Class {
	#name : #SpToolbarButtonPresenter,
	#superclass : #SpToolbarItemPresenter,
	#instVars : [
		'#label => SpObservableSlot',
		'#icon => SpObservableSlot',
		'#action',
		'#badge'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarButtonPresenter class >> adapterName [

	^ #ToolbarButtonAdapter
]

{ #category : #accessing }
SpToolbarButtonPresenter >> action [
	^ action
]

{ #category : #accessing }
SpToolbarButtonPresenter >> action: anObject [
	action := anObject
]

{ #category : #accessing }
SpToolbarButtonPresenter >> badge [
	^ badge
]

{ #category : #accessing }
SpToolbarButtonPresenter >> badge: aString [
	badge := aString
]

{ #category : #simulating }
SpToolbarButtonPresenter >> click [
	
	self execute
]

{ #category : #execution }
SpToolbarButtonPresenter >> execute [
	self action cull: self
]

{ #category : #accessing }
SpToolbarButtonPresenter >> icon [
	^ icon
]

{ #category : #accessing }
SpToolbarButtonPresenter >> icon: anObject [
	icon := anObject
]

{ #category : #initialization }
SpToolbarButtonPresenter >> initialize [
	super initialize.
]

{ #category : #accessing }
SpToolbarButtonPresenter >> label [
	^ label
]

{ #category : #accessing }
SpToolbarButtonPresenter >> label: anObject [
	label := anObject
]

{ #category : #'api-events' }
SpToolbarButtonPresenter >> whenIconChangedDo: aBlock [ 

	self property: #icon whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpToolbarButtonPresenter >> whenLabelChangedDo: aBlock [ 

	self property: #label whenChangedDo: aBlock 
]
