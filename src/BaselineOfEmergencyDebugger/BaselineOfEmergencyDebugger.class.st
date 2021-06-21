Class {
	#name : #BaselineOfEmergencyDebugger,
	#superclass : #BaselineOf,
	#category : #BaselineOfEmergencyDebugger
}

{ #category : #baselines }
BaselineOfEmergencyDebugger >> baseline: spec [
	<baseline> 
	
	spec for: #common do: [
		spec 
			package: 'EmergencyDebugger';
			package: 'EmergencyDebugger-Tests' with: [ spec requires: #('EmergencyDebugger') ].
			
		spec 
			group: 'default' with: #('Base' 'Tests');
			group: 'Base' with: #('EmergencyDebugger');
			group: 'Tests' with: #('EmergencyDebugger-Tests') ]
]
