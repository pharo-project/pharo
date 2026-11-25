"
Baseline managing the MenuRegistration project for the Pharo bootstrap.

Load type: 
- linear
"
Class {
	#name : 'BaselineOfMenuRegistration',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMenuRegistration',
	#package : 'BaselineOfMenuRegistration'
}

{ #category : 'baseline' }
BaselineOfMenuRegistration >> baseline: spec [ 
	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		"Menu item icons are form sets"
		spec baseline: 'Display' with: [ spec loads: #Core from: repository ].
		spec package: 'MenuRegistration' with: [ spec requires: #( 'Display' ) ].
	].
]
