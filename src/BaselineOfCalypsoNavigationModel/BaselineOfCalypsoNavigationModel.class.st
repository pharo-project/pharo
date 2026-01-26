Class {
	#name : 'BaselineOfCalypsoNavigationModel',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCalypsoNavigationModel',
	#package : 'BaselineOfCalypsoNavigationModel'
}

{ #category : 'baselines' }
BaselineOfCalypsoNavigationModel >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [ 
		spec
			package: #'Calypso-NavigationModel';
			package: #'Calypso-NavigationModel-Tests'
				with: [ spec requires: #( #'Calypso-NavigationModel' ) ].

	spec 
		group: 'default' with: #( 'Core' 'Tests' );
		group: 'Core' with: #('Calypso-NavigationModel');
		group: 'Tests' with: #('Core' 'Calypso-NavigationModel-Tests') ]
]
