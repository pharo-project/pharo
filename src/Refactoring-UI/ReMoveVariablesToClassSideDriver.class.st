"
I represent a driver that invokes RBMoveInstanceVariableToClassTransformation to move instance variables to their class side
"
Class {
	#name : 'ReMoveVariablesToClassSideDriver',
	#superclass : 'ReInteractionDriver',
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
ReMoveVariablesToClassSideDriver >> runRefactoring [

	self applyChanges
]

{ #category : 'execution' }
ReMoveVariablesToClassSideDriver >> scopes: scopesList variables: variables [

	scopes := scopesList.
	model := self refactoringScopeOn: scopesList first.
	refactoring := RBCompositeTransformation new
		               model: model;
		               transformations: (variables collect: [ :v |
						                RBMoveInstanceVariableToClassTransformation
							                model: model
							                variable: v name
							                fromClass: v definingClass
							                toClass: v definingClass classSide ])
]
