Class {
	#name : #DynamicMockPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'dynamicPresenter',
		'builder'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #specs }
DynamicMockPresenter class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add: #dynamicPresenter;
		yourself
]

{ #category : #accessing }
DynamicMockPresenter >> dynamicPresenter [
	^ dynamicPresenter
]

{ #category : #accessing }
DynamicMockPresenter >> dynamicPresenter: anObject [
	dynamicPresenter := anObject
]

{ #category : #initialization }
DynamicMockPresenter >> initializeWidgets [
	dynamicPresenter := self newNullPresenter.
	self rebuildDynamicPresenter
]

{ #category : #'dynamic presenter' }
DynamicMockPresenter >> rebuildDynamicPresenter [

	builder := DynamicPresentersListBuilder new
		modelObjects: (1 to: 15);
		presenter: ButtonPresenter configuredAs: [ :presenter :modelObject | presenter label: modelObject asString ];
		layoutBuilder: (DynamicLeftToRightColumnsLayout columns: 5);
		yourself.

	self newDynamicPresentersListIn: #dynamicPresenter usingBuilder: builder
]
