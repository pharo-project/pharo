"
I'm an abstract SpPresenter

my subclass have to define 
	addAllItems
	addItems 
	nextAction
	removeAllItems
	removeItems 

I define 
	label of button
	 
    Instance Variables
	addAllButton:		<SPButtonPresenter>
	addButton:		<SPButtonPresenter>
	nextButton:		<SPButtonPresenter>
	removeAllButton:		<SPButtonPresenter>
	removeButton:		<SPButtonPresenter>
	
	selectedPresenter:		<Object>
	toSelectPresenter:		<Object>
			those 2 instance Variable will 
"
Class {
	#name : #SelectBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'selectedPresenter',
		'toSelectPresenter',
		'addAllButton',
		'addButton',
		'nextButton',
		'removeAllButton',
		'removeButton'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
SelectBrowser class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpPanedLayout newHorizontal
				add: #toSelectPresenter;
				add: #selectedPresenter;
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: #addButton;
				add: #addAllButton;
				add: #removeButton;
				add: #removeAllButton;
				add: #nextButton;
				yourself)
			withConstraints: [ :contraint | contraint height: 25 ];
		yourself
]

{ #category : #actions }
SelectBrowser >> addAllItems [

	self subclassResponsibility 
]

{ #category : #actions }
SelectBrowser >> addItems [
	self subclassResponsibility
]

{ #category : #initialization }
SelectBrowser >> initializeWidgets [
	addButton := self newButton.
	addAllButton := self newButton.
	removeButton := self newButton.
	removeAllButton := self newButton.
	nextButton := self newButton.

	addButton
		label: 'Add';
		action: [ self addItems ].
	addAllButton
		label: 'Add all';
		action: [ self addAllItems ].
	removeButton
		label: 'Remove';
		action: [ self removeItems ].
	removeAllButton
		label: 'Remove all';
		action: [ self removeAllItems ].
	nextButton
		label: 'Next';
		action: [ self nextAction ].

	self focusOrder
		add: addButton;
		add: addAllButton;
		add: removeButton;
		add: removeAllButton
]

{ #category : #initialization }
SelectBrowser >> initializeWindow: aWindowPresenter [
	aWindowPresenter initialExtent: 450 @ 360
]

{ #category : #accessing }
SelectBrowser >> items: aCollection [
	"Set the elements to be analysed"
	
	toSelectPresenter items: (self sort: aCollection)

]

{ #category : #actions }
SelectBrowser >> nextAction [
	self subclassResponsibility

]

{ #category : #actions }
SelectBrowser >> removeAllItems [
	self subclassResponsibility
]

{ #category : #actions }
SelectBrowser >> removeItems [
	self subclassResponsibility
]

{ #category : #private }
SelectBrowser >> sort: aCollection [
	^ aCollection sorted: #asString ascending
]
