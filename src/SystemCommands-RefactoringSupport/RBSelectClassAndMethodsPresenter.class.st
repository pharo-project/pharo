Class {
	#name : #RBSelectClassAndMethodsPresenter,
	#superclass : #RBAbstractSelectionPresenter,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBSelectClassAndMethodsPresenter class >> title [ 
	^ 'Methods'
]

{ #category : #attributes }
RBSelectClassAndMethodsPresenter >> columnName [ 
	^ 'Methods'
]

{ #category : #attributes }
RBSelectClassAndMethodsPresenter >> columnSelector [
	^ #selector
]
