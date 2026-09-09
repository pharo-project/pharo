"
I handle the interaction for the user when calling `Abstract Instance Variable` refactoring
"
Class {
	#name : 'ReAbstractInstanceVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variableName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReAbstractInstanceVariableDriver class >> driverName [

	^ 'Abstract instance variable'
]

{ #category : 'configuration' }
ReAbstractInstanceVariableDriver >> instantiateRefactoring [

	^ ReAbstractInstanceVariableRefactoring variable: variableName class: class
]

{ #category : 'instance creation' }
ReAbstractInstanceVariableDriver >> scopes: aScope name: aName class: aClass [

	scopes := aScope.
	model := self refactoringScopeOn: scopes first.
	variableName := aName.
	class := aClass
]
