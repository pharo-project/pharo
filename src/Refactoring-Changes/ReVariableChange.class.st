"
I am the baseclass for all refactoring changes for all kind of variable changes, adding or removing class,  instance variables and pool variables.
I only hold the name of the actual variable. The concrete kind of variable and how the defintion for adding this variable looks like, is implemented
by my subclasses.
"
Class {
	#name : 'ReVariableChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'variable'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReVariableChange class >> add: aVariable to: aBehavior [
	"This should only be called on the Add*Change subclasses, but is here so we don't need to copy it to all subclasses"

	^ self new class: aBehavior variable: aVariable
]

{ #category : 'instance creation' }
ReVariableChange class >> remove: aVariable from: aBehavior [
	"This should only be called on the Remove*Change subclasses, but is here so we don't need to copy it to all subclasses"

	^ self new class: aBehavior variable: aVariable
]

{ #category : 'comparing' }
ReVariableChange >> = aRefactoryVariableChange [

	^ super = aRefactoryVariableChange and: [
		  self variable = aRefactoryVariableChange variable ]
]

{ #category : 'converting' }
ReVariableChange >> asText [

	^ self textToDisplay
]

{ #category : 'private' }
ReVariableChange >> changeObject [

	^ self variable
]

{ #category : 'private' }
ReVariableChange >> changeSymbol [

	self subclassResponsibility
]

{ #category : 'initialization' }
ReVariableChange >> class: aBehavior variable: aString [

	self changeClass: aBehavior.
	variable := aString
]

{ #category : 'private' }
ReVariableChange >> generateChanges [

	self changeClass perform: self changeSymbol with: self changeObject
]

{ #category : 'comparing' }
ReVariableChange >> hash [

	^ self changeClassName hash bitXor: self variable hash
]

{ #category : 'printing' }
ReVariableChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPut: $ ;
		nextPutAll: self changeSymbol;
		nextPut: $ ;
		print: self variable
]

{ #category : 'private' }
ReVariableChange >> variable [

	^ variable
]
