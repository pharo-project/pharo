"
I'm a model for user interaction for the move method to class refactoring.
"
Class {
	#name : 'ReMoveMethodToClassDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'method'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReMoveMethodToClassDriver class >> driverName [

	^ 'Move method to class'
]

{ #category : 'accessing' }
ReMoveMethodToClassDriver class >> shouldPreviewChanges [

	^ false 
]

{ #category : 'configuration' }
ReMoveMethodToClassDriver >> instantiateRefactoring [

	^ ReMoveMethodToClassRefactoring model: model method: method class: class
]

{ #category : 'instance creation' }
ReMoveMethodToClassDriver >> scopes: aScope method: aMethod class: aClass [

	scopes := aScope.
	model := self refactoringScopeOn: scopes first.
	method := aMethod.
	class := aClass
]
