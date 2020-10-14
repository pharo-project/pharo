"
A presenter to create a menu button (a button who exposes a menu instead having an action) in a `SpToolbarPresenter`.
"
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

{ #category : #documentation }
SpToolbarMenuButtonPresenter class >> documentFactoryMethodSelector [

	^ #newToolbarMenuButton
]

{ #category : #initialization }
SpToolbarMenuButtonPresenter >> initialize [

	super initialize.
	self addStyle: 'button'
]

{ #category : #api }
SpToolbarMenuButtonPresenter >> menu [
	"Answer the menu associated to this menu button. 
	 It can also be a block (or valuable) which will answer an instance of `SpMenuPresenter`
	 when evaluated. "

	^ menu
]

{ #category : #api }
SpToolbarMenuButtonPresenter >> menu: aMenuOrBlock [
	"Set the menu associated to this menu button. 
	 It can also be a block (or valuable) which will answer an instance of `SpMenuPresenter`
	 when evaluated. "

	menu := aMenuOrBlock
]
