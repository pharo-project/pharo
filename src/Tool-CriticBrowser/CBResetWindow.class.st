"
I am a UI to reset the currently analysed model.

* If you select 'Reset Current Configuration', I will open a CBPackagesChooserPresenter to set a new configuration.
* If you select 'Uses Current Configuration', I will open a CBCritiqueBrowser with the last configuration of selected packages and selected rules.

You can open me whit:
`CBResetWindow open`.
"
Class {
	#name : #CBResetWindow,
	#superclass : #SpPresenter,
	#instVars : [
		'resetModel',
		'currentConfigurationModel'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
CBResetWindow class >> defaultSpec [

	^ SpBoxLayout newTopToBottom
		  add: (SpBoxLayout newLeftToRight
				   add: #resetModel;
				   add: #currentConfigurationModel;
				   yourself)
		  height: 25
]

{ #category : #specs }
CBResetWindow class >> open [

	<example>
	^ self new openWithSpec
]

{ #category : #specs }
CBResetWindow class >> title [
	^ 'Critique Browser' 
]

{ #category : #accessing }
CBResetWindow >> currentConfigurationModel [
	^ currentConfigurationModel
]

{ #category : #initialization }
CBResetWindow >> initializePresenters [

	resetModel := self newButton.
	currentConfigurationModel := self newButton.

	self setFocusOrder.
	self setResetModel.
	self setCurrentConfigurationModel
]

{ #category : #api }
CBResetWindow >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title:  self title;
		initialExtent: 450@30
]

{ #category : #initialization }
CBResetWindow >> openCurrentConfiguration [

	CBCritiqueBrowser openOnWorkingConfiguration:
		  CBCritiqueWorkingConfiguration current
]

{ #category : #initialization }
CBResetWindow >> openPackagesChooser [

	^ CBSelectPackage open
]

{ #category : #accessing }
CBResetWindow >> resetModel [
	^ resetModel
]

{ #category : #initialization }
CBResetWindow >> setCurrentConfigurationModel [

	currentConfigurationModel
		state: false;
		label: 'Uses Current Configuration';
		action: [ self openCurrentConfiguration. self delete ]
]

{ #category : #initialization }
CBResetWindow >> setFocusOrder [

	self focusOrder
		add: resetModel;
		add: currentConfigurationModel
]

{ #category : #initialization }
CBResetWindow >> setResetModel [

	resetModel
		state: false;
		label: 'Reset Current Configurartion';
		action: [ 
			self openPackagesChooser.
			self delete ]
]
