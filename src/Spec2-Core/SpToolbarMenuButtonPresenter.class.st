Class {
	#name : #SpToolbarMenuButtonPresenter,
	#superclass : #SpToolbarButtonPresenter,
	#instVars : [
		'menu'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarMenuButtonPresenter class >> adapterName [

	^ #ToolbarMenuButtonAdapter
]

{ #category : #initialization }
SpToolbarMenuButtonPresenter >> initialize [

	super initialize.
	self addStyle: 'button'
]

{ #category : #accessing }
SpToolbarMenuButtonPresenter >> menu [
	^ menu
]

{ #category : #accessing }
SpToolbarMenuButtonPresenter >> menu: anObject [
	menu := anObject
]
