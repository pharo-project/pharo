"
A simple selection browser.
"
Class {
	#name : #SelectBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'toSelectModel',
		'selectedModel',
		'addButtonModel',
		'addAllButtonModel',
		'removeButtonModel',
		'removeAllButtonModel',
		'nextButtonModel'
	],
	#category : #'Tool-CriticBrowser-Applier'
}

{ #category : #specs }
SelectBrowser class >> defaultSpec [

	"Please do not use this method. We do not use this class anymore"
	<spec>
	self deprecated: 'Do not use this method'.
	
	^ SpecLayout composed
		newColumn: [:c |
			c
			newRow: [:r |
				r 
					add: #toSelectModel;
					addSplitter;
					add:  #selectedModel];
			newRow: [:r |
				r 
					add:#addButtonModel;
					add:#addAllButtonModel;
					add:#removeButtonModel;
					add:#removeAllButtonModel;
					add: #nextButtonModel ] height: 25]	
]

{ #category : #'as yet unclassified' }
SelectBrowser class >> isDeprecated [ 
	"We do not need this class anymore" 

	^true
]

{ #category : #accessing }
SelectBrowser >> addAllButtonModel [
	^ addAllButtonModel
]

{ #category : #private }
SelectBrowser >> addAllItems [

	self subclassResponsibility 
]

{ #category : #accessing }
SelectBrowser >> addButtonModel [
	^ addButtonModel
]

{ #category : #api }
SelectBrowser >> initialExtent [
	^   450@360
]

{ #category : #initialization }
SelectBrowser >> initializeWidgets [

	addButtonModel := self newButton.
	addAllButtonModel := self newButton. 
	removeButtonModel := self newButton.
	removeAllButtonModel := self newButton.
	nextButtonModel := self newButton.
		
	self setFocusOrder.

	self setAddButtonModel. 
	self setAddAllButtonModel. 
	self setRemoveButtonModel. 
	self setRemoveAllButtonModel. 
	self setNextButtonModel. 
]

{ #category : #accessing }
SelectBrowser >> items: aCollection [
	"Set the elements to be analysed"
	
	toSelectModel items: (self sort: aCollection)

]

{ #category : #api }
SelectBrowser >> nextAction [
	self subclassResponsibility

]

{ #category : #accessing }
SelectBrowser >> nextButtonModel [
	^ nextButtonModel
]

{ #category : #accessing }
SelectBrowser >> removeAllButtonModel [
	^ removeAllButtonModel
]

{ #category : #accessing }
SelectBrowser >> removeButtonModel [
	^ removeButtonModel
]

{ #category : #accessing }
SelectBrowser >> selectedModel [
	^ selectedModel
]

{ #category : #initialization }
SelectBrowser >> setAddAllButtonModel [
	
	addAllButtonModel
		state: false;
		label: 'Add All';
		action: [ self addAllItems]
	

]

{ #category : #initialization }
SelectBrowser >> setAddButtonModel [
	
	addButtonModel
		state: false;
		label: 'Add';
		action: [ self addItems ].
	

]

{ #category : #initialization }
SelectBrowser >> setFocusOrder [

	self focusOrder
		add: addButtonModel;
		add: addAllButtonModel;
		add: removeButtonModel;
		add: removeAllButtonModel.
		
]

{ #category : #initialization }
SelectBrowser >> setNextButtonModel [
	
	nextButtonModel
		state: false;
		label: 'Next';
		action: [ self  nextAction ].
	

]

{ #category : #initialization }
SelectBrowser >> setRemoveAllButtonModel [
	
	removeAllButtonModel
		state: false;
		label: 'Remove All';
		action: [ self  removeAllItems ].
	

]

{ #category : #initialization }
SelectBrowser >> setRemoveButtonModel [
	
	removeButtonModel
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
	^ toSelectModel
]
