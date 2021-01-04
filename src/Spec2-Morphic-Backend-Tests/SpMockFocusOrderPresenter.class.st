Class {
	#name : #SpMockFocusOrderPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'firstPresenter',
		'secondPresenter',
		'thirdPresenter'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #specs }
SpMockFocusOrderPresenter class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: #firstPresenter;
		add: #secondPresenter;
		add: #thirdPresenter;
		yourself
]

{ #category : #accessing }
SpMockFocusOrderPresenter >> firstPresenter [

	^ firstPresenter
]

{ #category : #initialization }
SpMockFocusOrderPresenter >> initializePresenters [

	firstPresenter := self newButton label: 'First'.
	secondPresenter := self newButton label: 'Second'.
	thirdPresenter := self newButton label: 'Third'
]

{ #category : #accessing }
SpMockFocusOrderPresenter >> secondPresenter [

	^ secondPresenter
]

{ #category : #accessing }
SpMockFocusOrderPresenter >> thirdPresenter [

	^ thirdPresenter
]
