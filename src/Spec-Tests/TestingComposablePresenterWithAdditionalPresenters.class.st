"
This presenter contains several subpresenters that are stored in additionalSubpresentersMap plus one presenter that is stored in instance variable but does not have any public accessors.
"
Class {
	#name : #TestingComposablePresenterWithAdditionalPresenters,
	#superclass : #ComposablePresenter,
	#instVars : [
		'subpresenter1',
		'subpresenter2',
		'subpresenter3',
		'subpresenter4'
	],
	#classInstVars : [
		'subpresenter4'
	],
	#category : #'Spec-Tests-Utils'
}

{ #category : #accessing }
TestingComposablePresenterWithAdditionalPresenters class >> defaultSpec [
	<spec>

	| aLayout |
	
	aLayout := SpecLayout composed newColumn: [ :row |
		self keys do: [ :aKey |
			row add: aKey ].
		
		row add: #subpresenter4 ].
	
	 ^ aLayout
	
]

{ #category : #accessing }
TestingComposablePresenterWithAdditionalPresenters class >> keys [ 

	^ #(subpresenter1 subpresenter2 subpresenter3)
]

{ #category : #initialization }
TestingComposablePresenterWithAdditionalPresenters >> initializeWidgets [

	self class keys do: [ :aKey |
		self additionalSubpresentersMap at: aKey put: (self newButton label: aKey)].
	
	subpresenter4 := self newButton label: 'subpresenter4'
]
