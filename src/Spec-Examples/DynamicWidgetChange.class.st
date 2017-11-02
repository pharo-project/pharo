"
I am a dummy proto to see how easy it is to replace a subwidget with another one dynamically.

slef example
"
Class {
	#name : #DynamicWidgetChange,
	#superclass : #ComposablePresenter,
	#instVars : [
		'boolean',
		'button',
		'bottom'
	],
	#category : 'Spec-Examples'
}

{ #category : #specs }
DynamicWidgetChange class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		newRow: #button height: 25;
		newRow: #bottom top: 25;
		yourself
]

{ #category : #example }
DynamicWidgetChange class >> example [
	^ self new openWithSpec
]

{ #category : #accessing }
DynamicWidgetChange >> bottom [
	^ bottom
]

{ #category : #accessing }
DynamicWidgetChange >> button [
	^ button
]

{ #category : #private }
DynamicWidgetChange >> change [

	boolean := boolean not.

	boolean
		 ifTrue: [ self changeToList ]
		 ifFalse: [ self changeToButton ].
		
	self needRebuild: false.
	button needRebuild: false.
	
	self buildWithSpec
]

{ #category : #private }
DynamicWidgetChange >> changeToButton [

	self instantiatePresenters: 
		#( bottom ButtonPresenter ).
	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 9).
		
	bottom label: 'I am useless'.
]

{ #category : #private }
DynamicWidgetChange >> changeToList [

	self instantiatePresenters: 
		#( bottom #ListPresenter ).

	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10).
		
	bottom items: (1 to: 100) asOrderedCollection.
]

{ #category : #initialization }
DynamicWidgetChange >> initialize [

	super initialize.

	boolean := true
]

{ #category : #initialization }
DynamicWidgetChange >> initializeWidgets [

	button := self newButton.
	bottom := self newList.
		
	button 
		label: 'Change';
		action: [ self change ];
		state: false.
		
	bottom 
		items: (1 to: 10) asOrderedCollection.
]
