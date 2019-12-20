"
I am a dummy proto to see how easy it is to replace a subwidget with another one dynamically.

slef example
"
Class {
	#name : #SpDynamicWidgetChange,
	#superclass : #SpPresenter,
	#instVars : [
		'boolean',
		'button',
		'bottom'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpDynamicWidgetChange class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #button withConstraints: [ :constraints | constraints height: 25 ];
		add: #bottom;
		yourself
]

{ #category : #example }
SpDynamicWidgetChange class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #accessing }
SpDynamicWidgetChange >> bottom [
	^ bottom
]

{ #category : #accessing }
SpDynamicWidgetChange >> button [
	^ button
]

{ #category : #private }
SpDynamicWidgetChange >> change [

	boolean := boolean not.

	boolean
		 ifTrue: [ self changeToList ]
		 ifFalse: [ self changeToButton ].
		
	self needRebuild: false.
	button needRebuild: false.
	
	self buildWithSpec
]

{ #category : #private }
SpDynamicWidgetChange >> changeToButton [

	self instantiatePresenters: 
		#( bottom #SpButtonPresenter ).
	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 9).
		
	bottom label: 'I am useless'.
]

{ #category : #private }
SpDynamicWidgetChange >> changeToList [

	self instantiatePresenters: 
		#( bottom #SpListPresenter ).

	button font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10).
		
	bottom items: (1 to: 100) asOrderedCollection.
]

{ #category : #initialization }
SpDynamicWidgetChange >> initialize [

	super initialize.

	boolean := true
]

{ #category : #initialization }
SpDynamicWidgetChange >> initializePresenters [

	button := self newButton.
	bottom := self newList.
		
	button 
		label: 'Change';
		action: [ self change ];
		state: false.
		
	bottom 
		items: (1 to: 10) asOrderedCollection.
]
