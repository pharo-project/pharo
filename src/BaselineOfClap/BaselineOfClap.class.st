Class {
	#name : #BaselineOfClap,
	#superclass : #BaselineOf,
	#category : #BaselineOfClap
}

{ #category : #baseline }
BaselineOfClap >> baseline: spec [
	<baseline>
	spec for: #common do: [ spec
		package: 'Clap-Core';
		package: 'Clap-CommandLine' with: [ spec requires: #('Clap-Core') ];
		package: 'Clap-Tests' with: [ spec requires: #('Clap-Core' 'Clap-Examples') ];
		package: 'Clap-Examples' with: [ spec requires: #('Clap-CommandLine') ];
		package: 'Clap-Commands-Pharo' with: [ spec requires: #('Clap-CommandLine') ];

		group: 'default' with: #(core development);
		group: 'core' with: #('Clap-Core' 'Clap-CommandLine');
		group: 'pharo' with: #('Clap-Commands-Pharo');
		group: 'development' with: #('Clap-Tests' 'Clap-Examples' 'Clap-Commands-Pharo') ]
]
