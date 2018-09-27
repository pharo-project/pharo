"
I am an abstract layout builder for a DynamicComposablePresenter built using DynamicPresentersListBuilder.
"
Class {
	#name : #DynamicPresentersListLayout,
	#superclass : #Object,
	#category : #'Spec-Core-Dynamic-Builder'
}

{ #category : #building }
DynamicPresentersListLayout >> buildFor: aDynamicPresentersListBuilder [
	aDynamicPresentersListBuilder
		createNullPresenters: (self nullPresentersNeededFor: aDynamicPresentersListBuilder).
	^ self buildLayoutFor: aDynamicPresentersListBuilder
]

{ #category : #building }
DynamicPresentersListLayout >> buildLayoutFor: aDynamicPresentersListBuilder [
	^ self subclassResponsibility
]

{ #category : #building }
DynamicPresentersListLayout >> instVarNamesFrom: aDynamicPresentersListBuilder [
	^ aDynamicPresentersListBuilder instVarNames , aDynamicPresentersListBuilder nullPresentersInstVarNames
]

{ #category : #building }
DynamicPresentersListLayout >> nullPresentersNeededFor: aDynamicPresentersListBuilder [
	"Compute the number of empty presenters required to fill additional space if the number of model objects modulo the number of columns/rows is not equals to 0."
	^ self subclassResponsibility
]
