Class {
	#name : #SpToolBarMenuButtonPresenter,
	#superclass : #SpToolBarButtonPresenter,
	#instVars : [
		'menu'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarMenuButtonPresenter class >> adapterName [

	^ #ToolBarMenuButtonAdapter
]

{ #category : #initialization }
SpToolBarMenuButtonPresenter >> initialize [

	super initialize.
	self addStyle: 'button'
]

{ #category : #accessing }
SpToolBarMenuButtonPresenter >> menu [
	^ menu
]

{ #category : #accessing }
SpToolBarMenuButtonPresenter >> menu: anObject [
	menu := anObject
]
