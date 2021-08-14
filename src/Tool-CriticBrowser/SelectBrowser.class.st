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

{ #category : #'as yet unclassified' }
SelectBrowser class >> isDeprecated [ 
	"We do not need this class anymore" 

	^true
]
