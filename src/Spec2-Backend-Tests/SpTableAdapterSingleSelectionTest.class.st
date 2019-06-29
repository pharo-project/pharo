Class {
	#name : #SpTableAdapterSingleSelectionTest,
	#superclass : #SpAbstractListAdapterSingleSelectionTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTableAdapterSingleSelectionTest >> classToTest [
	^ SpTablePresenter
]

{ #category : #running }
SpTableAdapterSingleSelectionTest >> initializeTestedInstance [ 

	super initializeTestedInstance.
	presenter addColumn: (SpStringTableColumn evaluated: [:e | e displayString]).
]
