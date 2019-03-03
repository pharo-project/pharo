Class {
	#name : #SpecMockPesenterWithoutGetter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'buttonPresenter'
	],
	#category : #'Spec-Tests-Utils'
}

{ #category : #specs }
SpecMockPesenterWithoutGetter class >> defaultSpec [
	^ SpecLayout composed newColumn: [ :col | col newRow: [ :row | row add: #buttonPresenter ] ]
]

{ #category : #initialization }
SpecMockPesenterWithoutGetter >> initializeWidgets [
	buttonPresenter := self newButton
]
