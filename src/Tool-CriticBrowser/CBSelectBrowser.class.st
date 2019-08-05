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
	#name : #CBSelectBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'selectedPresenter',
		'toSelectPresenter',
		'buttonBarPresenter'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
CBSelectBrowser class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpPanedLayout newHorizontal
				add: #toSelectPresenter;
				add: #selectedPresenter;
				yourself);
		add: #buttonBarPresenter
			withConstraints: [ :contraint | contraint height: 25 ];
		yourself
]

{ #category : #actions }
CBSelectBrowser >> addAllItems [

	self subclassResponsibility 
]

{ #category : #actions }
CBSelectBrowser >> addItems [
	self subclassResponsibility
]

{ #category : #initialization }
CBSelectBrowser >> initializeWidgets [
	buttonBarPresenter := self newActionBar.
	
	buttonBarPresenter
		add:
			(self newButton
				label: 'Add';
				help: 'Add an item to the selectedItem';
				action: [ self addItems ];
				yourself);
		add:
			(self newButton
				label: 'Add all';
				help: 'Add all items to SelectedItem';
				action: [ self addAllItems ];
				yourself);
		add:
			(self newButton
				label: 'Remove';
				help: 'remove the selected item from selectedItem';
				action: [ self removeItems ];
				yourself);
		add:
			(self newButton
				label: 'Remove all';
				help: 'remove all items from selectedItem';
				action: [ self removeAllItems ];
				yourself);
		add:
			(self newButton
				label: 'Next';
				help: 'Next step';
				action: [ self removeAllItems ];
				yourself).
				
	self focusOrder
		add: toSelectPresenter;
		add: selectedPresenter;
		add: buttonBarPresenter
]

{ #category : #initialization }
CBSelectBrowser >> initializeWindow: aWindowPresenter [
	aWindowPresenter initialExtent: 450 @ 360
]

{ #category : #accessing }
CBSelectBrowser >> items: aCollection [
	"Set the elements to be analysed"
	
	toSelectPresenter items: (self sort: aCollection)

]

{ #category : #actions }
CBSelectBrowser >> nextAction [
	self subclassResponsibility

]

{ #category : #actions }
CBSelectBrowser >> removeAllItems [
	self subclassResponsibility
]

{ #category : #actions }
CBSelectBrowser >> removeItems [
	self subclassResponsibility
]

{ #category : #private }
CBSelectBrowser >> sort: aCollection [
	^ aCollection sorted: #asString ascending
]
