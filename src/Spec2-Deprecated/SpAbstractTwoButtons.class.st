"
I am an abstract Spec widget managing two ButtonPresenter.

I provide shortcuts to set easier the buttons
-  firstIcon:secondIcon: that take two name of icon and set the buttons with the corresponding icons.
- firstLabel:secondLabel: to set labels.
- firstAction:secondAction: to set blocks to perform.

I provide two layout on my class side, vertical and horizontal.
"
Class {
	#name : #SpAbstractTwoButtons,
	#superclass : #SpPresenter,
	#instVars : [
		'firstButton',
		'secondButton'
	],
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #deprecation }
SpAbstractTwoButtons class >> abstractExample [
	^ self new
]

{ #category : #specs }
SpAbstractTwoButtons class >> horizontal [
	<spec>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #firstButton;
				add: #secondButton ];
		yourself
]

{ #category : #deprecation }
SpAbstractTwoButtons class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #specs }
SpAbstractTwoButtons class >> vertical [
	<spec: #default>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #firstButton;
				add: #secondButton ];
		yourself
]

{ #category : #api }
SpAbstractTwoButtons >> firstAction: firstActionBlock secondAction: secondActionBlock [
	self firstButton action: firstActionBlock.
	self secondButton action: secondActionBlock
]

{ #category : #accessing }
SpAbstractTwoButtons >> firstButton [
	^ firstButton
]

{ #category : #api }
SpAbstractTwoButtons >> firstIcon: firstIconName secondIcon: secondIconName [
	self firstButton icon: (self iconNamed: firstIconName).
	self secondButton icon: (self iconNamed: secondIconName)
]

{ #category : #api }
SpAbstractTwoButtons >> firstLabel: firstLabel secondLabel: secondLabel [
	self firstButton label: firstLabel.
	self secondButton label: secondLabel
]

{ #category : #initialization }
SpAbstractTwoButtons >> initializeButtons [
self subclassResponsibility
]

{ #category : #initialization }
SpAbstractTwoButtons >> initializePresenters [
	firstButton := self instantiate: SpButtonPresenter.
	secondButton := self instantiate: SpButtonPresenter.
	self initializeButtons.
	self focusOrder
		add: firstButton;
		add: secondButton
]

{ #category : #accessing }
SpAbstractTwoButtons >> secondButton [
	^ secondButton
]
