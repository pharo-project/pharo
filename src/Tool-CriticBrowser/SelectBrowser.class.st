"
A simple selection browser.
"
Class {
	#name : #SelectBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'addAllButtonPresenter',
		'addButtonPresenter',
		'nextButtonPresenter',
		'removeAllButtonPresenter',
		'removeButtonPresenter',
		'selectedPresenter',
		'toSelectPresenter'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
SelectBrowser class >> defaultSpec [
	<spec>
	^ SpBoxLayout newVertical
		add:
			(SpPanedLayout newHorizontal
				add: #toSelectPresenter;
				add: #selectedPresenter;
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: #addButtonPresenter;
				add: #addAllButtonPresenter;
				add: #removeButtonPresenter;
				add: #removeAllButtonPresenter;
				add: #nextButtonPresenter;
				yourself)
			withConstraints: [ :contraint | contraint height: 25 ] ;
		yourself
]

{ #category : #accessing }
SelectBrowser >> addAllButtonModel [
	^ addAllButtonPresenter
]

{ #category : #private }
SelectBrowser >> addAllItems [

	self subclassResponsibility 
]

{ #category : #accessing }
SelectBrowser >> addButtonModel [
	^ addButtonPresenter
]

{ #category : #private }
SelectBrowser >> addItems [
	^ self subclassResponsibility
]

{ #category : #initialization }
SelectBrowser >> initializeWidgets [

	addButtonPresenter := self newButton.
	addAllButtonPresenter := self newButton. 
	removeButtonPresenter := self newButton.
	removeAllButtonPresenter := self newButton.
	nextButtonPresenter := self newButton.
		
	self setFocusOrder.

	self setAddButtonModel. 
	self setAddAllButtonModel. 
	self setRemoveButtonModel. 
	self setRemoveAllButtonModel. 
	self setNextButtonModel. 
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

{ #category : #api }
SelectBrowser >> nextAction [
	self subclassResponsibility

]

{ #category : #accessing }
SelectBrowser >> nextButtonModel [
	^ nextButtonPresenter
]

{ #category : #accessing }
SelectBrowser >> removeAllButtonModel [
	^ removeAllButtonPresenter
]

{ #category : #private }
SelectBrowser >> removeAllItems [
	^ self subclassResponsibility
]

{ #category : #accessing }
SelectBrowser >> removeButtonModel [
	^ removeButtonPresenter
]

{ #category : #private }
SelectBrowser >> removeItems [
	^ self subclassResponsibility
]

{ #category : #accessing }
SelectBrowser >> selectedModel [
	^ selectedPresenter
]

{ #category : #initialization }
SelectBrowser >> setAddAllButtonModel [
	
	addAllButtonPresenter
		state: false;
		label: 'Add All';
		action: [ self addAllItems]
	

]

{ #category : #initialization }
SelectBrowser >> setAddButtonModel [
	
	addButtonPresenter
		state: false;
		label: 'Add';
		action: [ self addItems ].
	

]

{ #category : #initialization }
SelectBrowser >> setFocusOrder [

	self focusOrder
		add: addButtonPresenter;
		add: addAllButtonPresenter;
		add: removeButtonPresenter;
		add: removeAllButtonPresenter.
		
]

{ #category : #initialization }
SelectBrowser >> setNextButtonModel [
	
	nextButtonPresenter
		state: false;
		label: 'Next';
		action: [ self  nextAction ].
	

]

{ #category : #initialization }
SelectBrowser >> setRemoveAllButtonModel [
	
	removeAllButtonPresenter
		state: false;
		label: 'Remove All';
		action: [ self  removeAllItems ].
	

]

{ #category : #initialization }
SelectBrowser >> setRemoveButtonModel [
	
	removeButtonPresenter
		state: false;
		label: 'Remove';
		action: [ self removeItems ].
	

]

{ #category : #private }
SelectBrowser >> sort: aCollection [
	^ aCollection asOrderedCollection sort: [:e1 :e2 | (e1 asString) < (e2 asString)].
]

{ #category : #accessing }
SelectBrowser >> toSelectModel [
	^ toSelectPresenter
]
