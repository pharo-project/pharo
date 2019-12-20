Class {
	#name : #BaselineOfBeacon,
	#superclass : #BaselineOf,
	#category : #BaselineOfBeacon
}

{ #category : #baselines }
BaselineOfBeacon >> baseline: spec [
	<baseline>

	spec for: #'common' do: [
		spec 
			package: #'Beacon-Core';
			package: #'Beacon-Core-GT' with: [
				spec requires: #('Beacon-Core' #'Beacon-ExtraSignals'). ];
			package: #'Beacon-Core-Tests' with: [
				spec requires: #('Beacon-Core' ). ];
			package: #'Beacon-SerializingLoggers' with: [ spec requires: #('Beacon-Core') ];
			package: #'Beacon-ExtraSignals' with: [ spec requires: #('Beacon-Core') ];
			package: #'Beacon-Extra-Tests' with: [ spec requires: #('Beacon-SerializingLoggers' 'Beacon-ExtraSignals') ];
			package: #'Beacon-Syslog' with: [ spec requires: #('Beacon-SerializingLoggers') ];
			package: #'Beacon-File' with: [ spec requires: #('Beacon-SerializingLoggers') ];
			package: #'Beacon-Zinc' with: [ spec requires: #(#'Beacon-Core') ].
		spec 
			group: 'Core' with: #('Beacon-Core' );
			group: 'CoreTests' with: #('Beacon-Core' 'Beacon-Core-Tests');
			group: 'default' with: #('Beacon-Core' 'Beacon-Core-Tests' 'Beacon-SerializingLoggers' 'Beacon-Core-GT' 'Beacon-ExtraSignals' 'Beacon-Extra-Tests' 'Beacon-Syslog' 'Beacon-File' 'Beacon-Zinc'). ].

]
