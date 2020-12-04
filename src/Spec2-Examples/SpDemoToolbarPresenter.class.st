"
This demo shows a tool bar.
"
Class {
	#name : #SpDemoToolbarPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'toolBar',
		'text'
	],
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoToolbarPresenter class >> defaultSpec [

	^ SpBoxLayout newVertical 
		add: #toolBar expand: false fill: false padding: 0;
		add: #text;
		yourself
]

{ #category : #initialization }
SpDemoToolbarPresenter >> initializePresenters [
	
	toolBar := self newToolbar
		addItem: (SpToolbarButtonPresenter new
			label: 'Ok';
			icon: (self iconNamed: #smallOk);
			help: 'Ok';
			action: [ self inform: 'Ok' ];
			yourself);
		addItem: (SpToolbarButtonPresenter new
			label: 'Cancel';
			icon: (self iconNamed: #smallCancel);
			help: 'Cancel';
			action: [ self inform: 'Cancel' ];
			yourself);
		yourself.
	text := self newText
]
