Class {
	#name : #BaselineOfCommander2,
	#superclass : #BaselineOf,
	#category : #BaselineOfCommander2
}

{ #category : #baselines }
BaselineOfCommander2 >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec 
			package: 'Commander2';
			package: 'Commander2-Deprecations' with: [ spec requires: #('Commander2') ];
			package: 'Commander2-UI' with: [ spec requires: #('Commander2') ];
			package: 'Commander2-Tests' with: [ spec requires: #('Commander2') ];
			package: 'Commander2-UI-Tests' with: [ spec requires: #('Commander2-UI') ];
			package: 'Commander2-V2ReleaseMigrator'.
		spec
			group: 'core' with: #('Commander2' 'Commander2-Deprecations');
			group: 'ui' with: #('Commander2-UI');
			group: 'tests' with: #('core' 'ui' 'Commander2-Tests' 'Commander2-UI-Tests');
			group: 'migration' with: #('Commander2-V2ReleaseMigrator');
			group: 'default' with: #('core' 'ui' 'tests'). ]
]
