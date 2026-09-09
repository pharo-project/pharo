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
	spec for: #common do: [
		spec
			package: 'Clap-Core';
			package: 'Clap-Commands-Pharo' with: [ spec requires: #( 'Clap-Core' ) ];
			package: 'Clap-Examples' with: [ spec requires: #( 'Clap-Core' ) ];
			package: 'Clap-UI' with: [ spec requires: #( 'Clap-Core' ) ];
			package: 'Clap-Tests' with: [ spec requires: #( 'Clap-Core' 'Clap-Examples' ) ];
			package: 'Clap-UI-Tests' with: [ spec requires: #( 'Clap-UI' 'Clap-Examples' ) ];
			
			group: 'default' with: #( core development );
			group: 'core' with: #( 'Clap-Core' );
			group: 'pharo' with: #( 'Clap-Commands-Pharo' );
			group: 'examples' with: #( 'Clap-Examples' );
			group: 'tests' with: #( 'Clap-Tests' );
			group: 'development' with: #( core pharo examples tests) ;
			group: 'ui' with: #( 'Clap-UI' 'Clap-UI-Tests' ) ]

]
