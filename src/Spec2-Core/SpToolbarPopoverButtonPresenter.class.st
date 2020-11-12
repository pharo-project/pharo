"
A presenter to create a popover button (a button who exposes a popover instead having an action) in a `SpToolbarPresenter`.
"
Class {
	#name : #SpToolbarPopoverButtonPresenter,
	#superclass : #SpToolbarButtonPresenter,
	#instVars : [
		'content'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarPopoverButtonPresenter class >> adapterName [

	^ #ToolbarPopoverButtonAdapter
]

{ #category : #documentation }
SpToolbarPopoverButtonPresenter class >> documentFactoryMethodSelector [

	^ #newToolbarPopoverButton
]

{ #category : #api }
SpToolbarPopoverButtonPresenter >> content [
	"Answer the content presenter associated to this popover button. 
	 It can also be a block (or valuable) which will answer an instance of `SpPresenter`
	 when evaluated. "

	^ content
]

{ #category : #api }
SpToolbarPopoverButtonPresenter >> content: aPresenterOrBlock [
	"Set the presenter associated to this popover button. 
	 It can also be a block (or valuable) which will answer an instance of `SpPresenter`
	 when evaluated. "

	content := aPresenterOrBlock
]
