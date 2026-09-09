Class {
	#name : 'BaselineOfSystemSessionManager',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemSessionManager',
	#package : 'BaselineOfSystemSessionManager'
}

{ #category : 'baselines' }
BaselineOfSystemSessionManager >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'System-SessionManager';
				package: 'System-SessionManager-Tests' with: [ spec requires: #( 'System-SessionManager' ) ].

			spec
				group: 'Core' with: { 'System-SessionManager' };
				group: 'Tests' with: { 'System-SessionManager-Tests' } ]
]
