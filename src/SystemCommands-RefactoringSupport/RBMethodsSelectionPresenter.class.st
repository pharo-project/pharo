Class {
	#name : #RBMethodsSelectionPresenter,
	#superclass : #RBItemsSelectionPresenter,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #examples }
RBMethodsSelectionPresenter class >> example [
	<script>
	^ (self label: 'example ...' 
		withItems: SycExtractMethodCommand methods
		selecting: {} )
]

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
