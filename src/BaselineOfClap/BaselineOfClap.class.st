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
 		
		"Should be declared but already loaded in current Kernel image"
		"spec baseline: 'SUnit' with: [ spec loads: 'Core' ]."
		
		spec
			package: 'Clap-Core';
			package: 'Clap-Commands-Pharo' with: [ spec requires: #( 'Clap-Core' ) ];
			package: 'Clap-Examples' with: [ spec requires: #( 'Clap-Core' ) ];
			package: 'Clap-UI' with: [ spec requires: #( 'Clap-Core' ) ];
			package: 'Clap-Tests' with: [ 
				spec requires: #( "'SUnit'" 'Clap-Core' 'Clap-Examples' 'Clap-Commands-Pharo' ) ];
			package: 'Clap-UI-Tests' with: [ spec requires: #( 'Clap-UI' 'Clap-Examples' ) ];
			
			group: 'default' with: #( Core Development );
			group: 'Core' with: #( 'Clap-Core' );
			group: 'Pharo' with: #( 'Clap-Commands-Pharo' );
			group: 'Examples' with: #( 'Clap-Examples' );
			group: 'Tests' with: #( 'Clap-Tests' );
			group: 'Development' with: #( Core Pharo Examples Tests) ;
			group: 'UI' with: #( 'Clap-UI' 'Clap-UI-Tests' ) ]

]
