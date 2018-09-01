"
I am an abstract layout builder for a DynamicComposablePresenter built using DynamicPresentersListBuilder.
"
Class {
	#name : #DynamicPresentersListLayout,
	#superclass : #Object,
	#category : #'Spec-Core-Dynamic-Builder'
}

{ #category : #building }
DynamicPresentersListLayout >> buildLayoutFor: aDynamicPresentersListBuilder [
	^ self subclassResponsibility
]

{ #category : #building }
DynamicPresentersListLayout >> instVarNamesFrom: aDynamicPresentersListBuilder [
	^ aDynamicPresentersListBuilder instVarNames , aDynamicPresentersListBuilder nullPresentersInstVarNames
]
