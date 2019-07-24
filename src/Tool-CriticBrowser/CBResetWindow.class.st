"
A ResetWindow is simple UI to reset the currently analysed model.
"
Class {
	#name : #CBResetWindow,
	#superclass : #SpPresenter,
	#instVars : [
		'explanationPresenter'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
CBResetWindow class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #explanationPresenter;
		yourself
]

{ #category : #initialization }
CBResetWindow >> initializeDialogWindow: aDialogPresenter [
	aDialogPresenter
		addButton: 'Reset'
			do: [ :presenter | 
			CBSelectPackageBrowser open.
			presenter close ];
		addButton: 'Use current'
			do: [ :presenter | 
			CBCriticBrowser openOnWorkingConfiguration: CBCriticWorkingConfiguration current.
			presenter close ];
		addButton: 'Cancel' do: [ :presenter | presenter close ]
]

{ #category : #initialization }
CBResetWindow >> initializeWidgets [
	explanationPresenter := self newText.

	explanationPresenter
		text: 'A configuration was previously created.
Do you want to reset it or use the current configuration?';
		disable
]

{ #category : #initialization }
CBResetWindow >> initializeWindow: aWindowPresenter [
	aWindowPresenter title: 'Critic Browser'
]
