"
This demo shows a tool bar.
"
Class {
	#name : #SpDemoToolBarPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'toolBar',
		'text'
	],
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoToolBarPresenter class >> defaultSpec [

	^ SpBoxLayout newVertical 
		add: #toolBar expand: false fill: false padding: 0;
		add: #text;
		yourself
]

{ #category : #initialization }
SpDemoToolBarPresenter >> initializePresenters [
	
	toolBar := self newToolBar
		addItem: (SpToolBarButton new
			label: 'Ok';
			icon: (self iconNamed: #smallOk);
			help: 'Ok';
			action: [ self inform: 'Ok' ];
			yourself);
		addItem: (SpToolBarButton new
			label: 'Cancel';
			icon: (self iconNamed: #smallCancel);
			help: 'Cancel';
			action: [ self inform: 'Cancel' ];
			yourself);
		yourself.
	text := self newText
]
