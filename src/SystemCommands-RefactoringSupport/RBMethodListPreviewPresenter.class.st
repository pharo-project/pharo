Class {
	#name : #RBMethodListPreviewPresenter,
	#superclass : #RBListPreviewPresenter,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBMethodListPreviewPresenter class >> title [ 
	^ 'Methods'
]

{ #category : #attributes }
RBMethodListPreviewPresenter >> columnName [ 
	^ 'Methods'
]

{ #category : #attributes }
RBMethodListPreviewPresenter >> columnSelector [
	^ #selector
]
