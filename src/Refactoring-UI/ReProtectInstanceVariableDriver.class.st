"
I handle the interaction for the user when calling `Protect Instance Variable` refactoring
"
Class {
	#name : 'ReProtectInstanceVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReProtectInstanceVariableDriver class >> driverName [

	^ 'Protect Instance Variable'
]

{ #category : 'configuration' }
ReProtectInstanceVariableDriver >> instantiateRefactoring [

	^ ReProtectInstanceVariableRefactoring variable: variable name class: class
]

{ #category : 'instance creation' }
ReProtectInstanceVariableDriver >> scopes: aScope variable: aVariable class: aClass [

	scopes := aScope.
	model := self refactoringScopeOn: scopes first.
	variable := aVariable.
	class := aClass
]
