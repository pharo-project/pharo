Class {
	#name : #SpDynamicMockPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'dynamicPresenter',
		'builder'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #specs }
SpDynamicMockPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #dynamicPresenter;
		yourself
]

{ #category : #accessing }
SpDynamicMockPresenter >> dynamicPresenter [
	^ dynamicPresenter
]

{ #category : #accessing }
SpDynamicMockPresenter >> dynamicPresenter: anObject [
	dynamicPresenter := anObject
]

{ #category : #initialization }
SpDynamicMockPresenter >> initializePresenters [
	dynamicPresenter := self newNullPresenter.
	self rebuildDynamicPresenter
]

{ #category : #'dynamic presenter' }
SpDynamicMockPresenter >> rebuildDynamicPresenter [

	builder := SpDynamicPresentersListBuilder new
		modelObjects: (1 to: 15);
		presenter: SpButtonPresenter configuredAs: [ :presenter :modelObject | presenter label: modelObject asString ];
		layoutBuilder: (SpDynamicLeftToRightColumnsLayout columns: 5);
		yourself.

	self newDynamicPresentersListIn: #dynamicPresenter usingBuilder: builder
]
