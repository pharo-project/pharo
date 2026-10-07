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
ReMoveMethodToClassDriver >> breakingChoices [
	
	| items |
	items := OrderedCollection new.
	items add: (ReBrowseMethodChoice new driver: self).
	items add: (ReMoveClasstoInstanceSideChoice new driver: self).
	^ items
]

{ #category : 'configuration' }
ReMoveMethodToClassDriver >> browseMethod [

	(Smalltalk tools toolNamed: #messageList) browseImplementorsOfAll: refactoring method selector
]

{ #category : 'configuration' }
ReMoveMethodToClassDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'Warning you may break your code';
		  message: 'One or more methods are already defined on the class side!';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
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
