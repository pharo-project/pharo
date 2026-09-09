"
I'm a model for user interaction for the replace message send transformation.
"
Class {
	#name : 'ReReplaceMessageSendDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'selector',
		'oldSelector',
		'class'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReReplaceMessageSendDriver class >> driverName [

	^ 'Replace Message Send'
]

{ #category : 'configuration' }
ReReplaceMessageSendDriver >> instantiateRefactoring [

	^ (ReReplaceMessageSendTransformation
		   model: model 
		   replaceMethod: oldSelector selector
		   in: class
		   to: selector selector
		   permutation: selector permutation 
		   inAllClasses: true) newArgs: selector newArgs
]

{ #category : 'initialization' }
ReReplaceMessageSendDriver >> scopes: refactoringScopes class: aClass selector: aSelector oldSelector: aSecondSelector [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	class := aClass.
	selector := aSelector.
	oldSelector := aSecondSelector
]
