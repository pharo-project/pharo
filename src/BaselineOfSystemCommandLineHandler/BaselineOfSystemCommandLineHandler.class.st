Class {
	#name : 'BaselineOfSystemCommandLineHandler',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemCommandLineHandler',
	#package : 'BaselineOfSystemCommandLineHandler'
}

{ #category : 'baselines' }
BaselineOfSystemCommandLineHandler >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'System-CommandLineHandler';
				package: 'System-CommandLineHandler-Tests' with: [ spec requires: #( 'System-CommandLineHandler') ].

			spec
				group: 'Core' with: { 'System-CommandLineHandler' };
				group: 'Tests' with: { 'System-CommandLineHandler-Tests' } ]
]
