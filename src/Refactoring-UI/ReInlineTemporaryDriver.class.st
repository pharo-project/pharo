"
I handle user interaction when invoking Inline Temporary Refactoring.
"
Class {
	#name : 'ReInlineTemporaryDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'selector',
		'interval',
		'class'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReInlineTemporaryDriver class >> driverName [

	^ 'inline Temporary Variable'
]

{ #category : 'configuration' }
ReInlineTemporaryDriver >> instantiateRefactoring [

	^ ReInlineTemporaryRefactoring inline: interval from: selector in: class
]

{ #category : 'instance creation' }
ReInlineTemporaryDriver >> scopes: refactoringScopes interval: anInterval selector: aSelector class: aClass [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	interval := anInterval.
	selector := aSelector.
	class := aClass
]
