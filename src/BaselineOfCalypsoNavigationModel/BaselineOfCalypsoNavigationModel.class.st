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
			baseline: 'ClassAnnotation' with: [
				spec
					repository: repository;
					loads: 'default' ];
			
			package: #'Calypso-NavigationModel';
			package: #'Calypso-NavigationModel-Tests'
				with: [ spec requires: #( #'Calypso-NavigationModel' ) ].

	spec 
		group: 'default' with: #( 'Core' 'Tests' );
		group: 'Core' with: #('ClassAnnotation');
		group: 'Tests' with: #('ClassAnnotation-Tests') ]
]
