"
A button with an icon and label associated with an action and a badge 
"
Class {
	#name : #SpToolBarButton,
	#superclass : #SpToolBarItemPresenter,
	#instVars : [
		'label',
		'icon',
		'action',
		'badge'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarButton class >> adapterName [

	^ #ToolBarButtonAdapter
]

{ #category : #accessing }
SpToolBarButton >> action [
	^ action
]

{ #category : #accessing }
SpToolBarButton >> action: anObject [
	action := anObject
]

{ #category : #accessing }
SpToolBarButton >> badge [
	^ badge
]

{ #category : #accessing }
SpToolBarButton >> badge: aString [
	badge := aString
]

{ #category : #simulating }
SpToolBarButton >> click [
	
	self execute
]

{ #category : #execution }
SpToolBarButton >> execute [
	self action cull: self
]

{ #category : #accessing }
SpToolBarButton >> icon [
	^ icon
]

{ #category : #accessing }
SpToolBarButton >> icon: anObject [
	icon := anObject
]

{ #category : #accessing }
SpToolBarButton >> label [
	^ label
]

{ #category : #accessing }
SpToolBarButton >> label: anObject [
	label := anObject
]
