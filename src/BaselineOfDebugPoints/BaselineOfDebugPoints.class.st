Class {
	#name : 'BaselineOfDebugPoints',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfDebugPoints',
	#package : 'BaselineOfDebugPoints'
}

{ #category : 'baselines' }
BaselineOfDebugPoints >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec
			baseline: 'Reflectivity'
			with: [ spec repository: (self packageRepositoryURLForSpec: spec) ].

		spec
			package: 'DebugPoints'
			with: [ spec requires: #( 'Reflectivity' ) ].

		spec
			package: 'DebugPoints-Tests'
			with: [ spec requires: #( 'DebugPoints' ) ].

		"Groups"
		spec
			group: 'Core' with: #( 'DebugPoints' );
			group: 'Tests' with: #( 'Core' 'DebugPoints-Tests' );
			group: 'default' with: #( 'Tests' ) ]
]
