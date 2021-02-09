Class {
	#name : #RBMethodsSelectionPresenter,
	#superclass : #RBItemsSelectionPresenter,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBMethodsSelectionPresenter class >> title [ 
	^ 'Methods'
]

{ #category : #attributes }
RBMethodsSelectionPresenter >> columnName [ 
	^ 'Methods'
]

{ #category : #attributes }
RBMethodsSelectionPresenter >> columnSelector [
	^ #selector
]
