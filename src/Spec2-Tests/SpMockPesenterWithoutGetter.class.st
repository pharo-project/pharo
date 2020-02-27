Class {
	#name : #SpMockPesenterWithoutGetter,
	#superclass : #SpPresenter,
	#instVars : [
		'buttonPresenter'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #specs }
SpMockPesenterWithoutGetter class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #buttonPresenter;
		yourself
]

{ #category : #initialization }
SpMockPesenterWithoutGetter >> initializePresenters [
	buttonPresenter := self newButton
]
