"
I am an abstract Spec widget managing two ButtonPresenter.

I provide shortcuts to set easier the buttons
-  firstIcon:secondIcon: that take two name of icon and set the buttons with the corresponding icons.
- firstLabel:secondLabel: to set labels.
- firstAction:secondAction: to set blocks to perform.

I provide two layout on my class side, vertical and horizontal.
"
Class {
	#name : #AbstractTwoButtons,
	#superclass : #ComposablePresenter,
	#instVars : [
		'firstButton',
		'secondButton'
	],
	#category : #Spec-PolyWidgets-Support
}

{ #category : #example }
AbstractTwoButtons class >> abstractExample [
	| example |
	example := self new.
	example
		title: self name asString , ' example';
		extent: 100 @ 100.
	^ example
]

{ #category : #specs }
AbstractTwoButtons class >> horizontal [
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #firstButton;
				add: #secondButton ];
		yourself
]

{ #category : #specs }
AbstractTwoButtons class >> vertical [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #firstButton;
				add: #secondButton ];
		yourself
]

{ #category : #api }
AbstractTwoButtons >> firstAction: firstActionBlock secondAction: secondActionBlock [
	self firstButton action: firstActionBlock.
	self secondButton action: secondActionBlock
]

{ #category : #accessing }
AbstractTwoButtons >> firstButton [
	^ firstButton
]

{ #category : #api }
AbstractTwoButtons >> firstIcon: firstIconName secondIcon: secondIconName [
	self firstButton icon: (self iconNamed: firstIconName).
	self secondButton icon: (self iconNamed: secondIconName)
]

{ #category : #api }
AbstractTwoButtons >> firstLabel: firstLabel secondLabel: secondLabel [
	self firstButton label: firstLabel.
	self secondButton label: secondLabel
]

{ #category : #initialization }
AbstractTwoButtons >> initializeButtons [
self subclassResponsibility
]

{ #category : #initialization }
AbstractTwoButtons >> initializeWidgets [
	firstButton := self instantiate: ButtonPresenter.
	secondButton := self instantiate: ButtonPresenter.
	self initializeButtons.
	self focusOrder
		add: firstButton;
		add: secondButton
]

{ #category : #accessing }
AbstractTwoButtons >> secondButton [
	^ secondButton
]
