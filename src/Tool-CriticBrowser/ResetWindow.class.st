"
A ResetWindow is simple UI to reset the currently analysed model.
"
Class {
	#name : #ResetWindow,
	#superclass : #ComposablePresenter,
	#instVars : [
		'resetModel',
		'currentConfigurationModel'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
ResetWindow class >> defaultSpec [
<spec>

	^SpecLayout composed
		newRow: [:r |
			r 
				add: #resetModel;
				add: #currentConfigurationModel
			]  height: 25
]

{ #category : #specs }
ResetWindow class >> title [
	^ 'Critic Browser' 
]

{ #category : #accessing }
ResetWindow >> currentConfigurationModel [
	^ currentConfigurationModel
]

{ #category : #api }
ResetWindow >> initialExtent [

	^   450@30
]

{ #category : #initialization }
ResetWindow >> initializeWidgets [

	resetModel := self newButton.
	currentConfigurationModel := self newButton.

	self setFocusOrder.
	self setResetModel. 
	self setCurrentConfigurationModel
	
]

{ #category : #accessing }
ResetWindow >> resetModel [
	^ resetModel
]

{ #category : #initialization }
ResetWindow >> setCurrentConfigurationModel [
	currentConfigurationModel  
		state: false;
		label: 'Uses Current Configuration';
		action: [ CriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current. 
				self delete ]
]

{ #category : #initialization }
ResetWindow >> setFocusOrder [

	self focusOrder
		add: resetModel;
		add: currentConfigurationModel
]

{ #category : #initialization }
ResetWindow >> setResetModel [
	resetModel 
		state: false;
		label: 'Reset Current Configurartion';
		action: [ SelectPackageBrowser open. 
				self delete ]
]
