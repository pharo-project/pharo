Class {
	#name : 'BaselineOfCalypsoNavigationModel',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCalypsoNavigationModel',
	#package : 'BaselineOfCalypsoNavigationModel'
}

{ #category : 'baselines' }
BaselineOfCalypsoNavigationModel >> baseline: spec [

	<baseline>
	
	spec for: #common do: [ 
		spec
			package: #'Calypso-NavigationModel';
			package: #'Calypso-NavigationModel-Tests'
				with: [ spec requires: #( #'Calypso-NavigationModel' ) ].

	spec 
		group: 'default' with: #( 'Core' 'Tests' );
		group: 'Core' with: #('ClassAnnotation');
		group: 'Tests' with: #('ClassAnnotation-Tests') ]
]
