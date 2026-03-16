"
This is the baseline for Clap - the Pharo command line handler
"
Class {
	#name : 'BaselineOfClap',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfClap',
	#package : 'BaselineOfClap'
}

{ #category : 'baselines' }
BaselineOfClap >> baseline: spec [
	<baseline>
	spec for: #common do: [ spec
		baseline: 'Okay' with: [ spec repository: 'github://cdlm/okay-st' ];

		package: 'Clap-Core';
		package: 'Clap-CommandLine' with: [ spec requires: #('Clap-Core') ];
		package: 'Clap-Commands-Pharo' with: [ spec requires: #('Clap-CommandLine') ];
		package: 'Clap-Examples' with: [ spec requires: #('Clap-CommandLine') ];
		package: 'Clap-Examples-Booklet' with: [ spec requires: #('Clap-CommandLine') ];
		package: 'Clap-Tests' with: [ spec requires: #('Clap-Core' 'Clap-Examples') ];
		package: 'Clap-Okay-Tests' with: [ spec requires: #('Clap-Core' 'Okay') ];

		group: 'default' with: #(core development);
		group: 'core' with: #('Clap-Core' 'Clap-CommandLine');
		group: 'pharo' with: #('Clap-Commands-Pharo');
		group: 'examples' with: #('Clap-Examples');
		group: 'tests' with: #('Clap-Tests');
		group: 'development' with: #(core pharo examples tests);
		group: 'development-full' with: #(development 'Clap-Examples-Booklet' 'Clap-Okay-Tests') ]
]
