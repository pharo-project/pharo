Class {
	#name : 'BaselineOfSystemFinalization',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemFinalization',
	#package : 'BaselineOfSystemFinalization'
}

{ #category : 'baselines' }
BaselineOfSystemFinalization >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'System-Finalization';
				package: 'System-Finalization-Tests' with: [ spec requires: #( 'System-Finalization' ) ].

			spec
				group: 'Core' with: { 'System-Finalization' };
				group: 'Tests' with: { 'System-Finalization-Tests' } ]
]
