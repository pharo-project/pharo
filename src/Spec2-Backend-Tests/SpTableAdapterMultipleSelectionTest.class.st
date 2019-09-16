Class {
	#name : #SpTableAdapterMultipleSelectionTest,
	#superclass : #SpAbstractListAdapterMultipleSelectionTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTableAdapterMultipleSelectionTest >> classToTest [
	^ SpTablePresenter
]

{ #category : #running }
SpTableAdapterMultipleSelectionTest >> initializeTestedInstance [

	super initializeTestedInstance.

	presenter addColumn: (SpStringTableColumn evaluated: [ :e | e ])
]
