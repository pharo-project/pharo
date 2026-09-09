"
I represent a driver that invokes RBMoveInstanceVariableToClassTransformation to move instance variables to their class side
"
Class {
	#name : 'ReMoveVariablesToClassSideDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReMoveVariablesToClassSideDriver class >> driverName [

	^ 'Move variable(s) to class side'
]

{ #category : 'initialization' }
ReMoveVariablesToClassSideDriver class >> scopes: scopesList variables: aCollectionOfVariables [

	^ self new scopes: scopesList variables: aCollectionOfVariables
]

{ #category : 'execution' }
ReMoveVariablesToClassSideDriver >> instantiateRefactoring [

	^ RBCompositeTransformation new
		  model: model;
		  transformations: (variables collect: [ :v |
						   RBMoveInstanceVariableToClassTransformation
							   model: model
							   variable: v name
							   fromClass: v definingClass
							   toClass: v definingClass classSide ])
]

{ #category : 'execution' }
ReMoveVariablesToClassSideDriver >> scopes: scopesList variables: aVariableCollection [

	scopes := scopesList.
	model := self refactoringScopeOn: scopesList first.
	variables:= aVariableCollection 
]
