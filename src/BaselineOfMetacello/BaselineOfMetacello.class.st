Class {
	#name : 'BaselineOfMetacello',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMetacello',
	#package : 'BaselineOfMetacello'
}

{ #category : 'baseline' }
BaselineOfMetacello >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec 
			package: 'ScriptingExtensions';
			package: 'FileSystem-Memory';
			package: 'Metacello-Base';
			package: 'Metacello-Core';
			package: 'Metacello-GitBasedRepository';
			package: 'MonticelloFileTree-Core';
			package: 'MonticelloFileTree-FileSystem-Utilities';
			package: 'STON-Core';
			package: 'Metacello-CommandLine'. 

		spec
			package: 'Metacello-TestsCore';
			package: 'Metacello-TestsMCCore';
			package: 'Metacello-TestsReference'.

		spec 
			group: 'Core' with: #('ScriptingExtensions' 'System-FileRegistry' 'FileSystem-Memory' 'PragmaCollector' 'System-FileRegistry' 'Metacello-Base' 'Metacello-Core' 'MonticelloFileTree-Core' 'MonticelloFileTree-FileSystem-Utilities' 'STON-Core' 'Metacello-GitBasedRepository' 'Metacello-CommandLine' );
			group: 'Tests' with: #( 'Metacello-TestsCore' 'Metacello-TestsMCCore' 'Metacello-TestsReference' );
			group: 'default' with: #('Core') ]
]
