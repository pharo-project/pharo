"
A ResetWindow is simple UI to reset the currently analysed model.
"
Class {
	#name : #ResetWindow,
	#superclass : #SpPresenter,
	#instVars : [
		'explanationPresenter'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
ResetWindow class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #explanationPresenter;
		yourself
]

{ #category : #initialization }
ResetWindow >> initializeDialogWindow: aDialogPresenter [
	aDialogPresenter
		addButton: 'Reset'
			do: [ :presenter | 
			SelectPackageBrowser open.
			presenter close ];
		addButton: 'Use current'
			do: [ :presenter | 
			CBCriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current.
			presenter close ];
		addButton: 'Cancel' do: [ :presenter | presenter close ]
]

{ #category : #initialization }
ResetWindow >> initializeWidgets [
	explanationPresenter := self newText.

	explanationPresenter
		text: 'A configuration was previously created.
Do you want to reset it or use the current configuration?';
		disable
]

{ #category : #initialization }
ResetWindow >> initializeWindow: aWindowPresenter [
	aWindowPresenter title: 'Critic Browser'
]
