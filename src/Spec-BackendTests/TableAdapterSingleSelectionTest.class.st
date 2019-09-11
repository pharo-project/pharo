Class {
	#name : #TableAdapterSingleSelectionTest,
	#superclass : #AbstractListAdapterSingleSelectionTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
TableAdapterSingleSelectionTest >> classToTest [
	^ TablePresenter
]

{ #category : #running }
TableAdapterSingleSelectionTest >> initializeTestedInstance [ 

	super initializeTestedInstance.
	presenter addColumn: (StringTableColumn evaluated: [:e | e displayString]).
]
