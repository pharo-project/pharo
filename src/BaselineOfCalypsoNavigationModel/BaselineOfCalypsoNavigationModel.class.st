Class {
	#name : 'BaselineOfCalypsoNavigationModel',
	#superclass : 'Object',
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
				with: [ spec requires: #( #'Calypso-NavigationModel' ) ] ]
]
