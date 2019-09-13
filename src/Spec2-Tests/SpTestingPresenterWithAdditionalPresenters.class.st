"
This presenter contains several subpresenters that are stored in additionalSubpresentersMap plus one presenter that is stored in instance variable but does not have any public accessors.
"
Class {
	#name : #SpTestingPresenterWithAdditionalPresenters,
	#superclass : #SpPresenter,
	#instVars : [
		'subpresenter1',
		'subpresenter2',
		'subpresenter3',
		'subpresenter4'
	],
	#classInstVars : [
		'subpresenter4'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #accessing }
SpTestingPresenterWithAdditionalPresenters class >> defaultSpec [
	<spec>
	| aLayout |
	aLayout := SpBoxLayout newVertical.

	self keys do: [ :aKey | aLayout add: aKey ].

	aLayout add: #subpresenter4.

	^ aLayout
]

{ #category : #accessing }
SpTestingPresenterWithAdditionalPresenters class >> keys [ 

	^ #(subpresenter1 subpresenter2 subpresenter3)
]

{ #category : #initialization }
SpTestingPresenterWithAdditionalPresenters >> initializePresenters [

	self class keys do: [ :aKey |
		self additionalSubpresentersMap at: aKey put: (self newButton label: aKey)].
	
	subpresenter4 := self newButton label: 'subpresenter4'
]
