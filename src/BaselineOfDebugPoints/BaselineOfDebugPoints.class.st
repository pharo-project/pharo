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
		spec package: 'DebugPoints'.

		spec
			package: 'DebugPoints-Tests'
			with: [ spec requires: #( 'DebugPoints' ) ].

		"Groups"
		spec
			group: 'Core' with: #( 'DebugPoints' );
			group: 'Tests' with: #( 'Core' 'DebugPoints-Tests' );
			group: 'default' with: #( 'Tests' ) ]
]
