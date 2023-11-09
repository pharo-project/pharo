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
			package: 'System-FileRegistry';
			package: 'FileSystem-Memory';
			package: 'StartupPreferences';
			package: 'ConfigurationCommandLineHandler-Core';
			package: 'PragmaCollector';
			package: 'System-FileRegistry';
			package: 'MonticelloConfigurations';
			package: 'Gofer-Core';
			package: 'Metacello-Base';
			package: 'Metacello-Bitbucket';
			package: 'Metacello-Core';
			package: 'Metacello-GitBasedRepository';
			package: 'Metacello-GitHub';
			package: 'Metacello-Gitlab';
			package: 'Metacello-MC';
			package: 'Metacello-Platform';

			package: 'MonticelloFileTree-Core';
			package: 'MonticelloFileTree-FileSystem-Utilities';
			package: 'STON-Core';
			package: 'MetacelloCommandLineHandler-Core'. 

		spec
			package: 'Metacello-TestsCore';
			package: 'Metacello-TestsMCCore';	"standalone"
			package: 'Metacello-TestsReference';	"standalone"
			package: 'Metacello-Gitlab-Tests'.

		spec 
			group: 'Core' with: #('ScriptingExtensions' 'System-FileRegistry' 'FileSystem-Memory' 'Regex-Core' 'StartupPreferences' 'ConfigurationCommandLineHandler-Core' 'PragmaCollector' 'System-FileRegistry' 'MonticelloConfigurations' 'Gofer-Core' 'Metacello-Base' 'Metacello-Core' 'MonticelloFileTree-Core' 'Metacello-MC' 'MonticelloFileTree-FileSystem-Utilities' 'STON-Core' 'Metacello-GitBasedRepository' 'Metacello-GitHub' 'Metacello-Gitlab' 'Metacello-Bitbucket' 'Metacello-Platform' 'MetacelloCommandLineHandler-Core');
			group: 'Tests' with: #( 'Metacello-TestsCore' 'Metacello-TestsMCCore' 'Metacello-TestsReference' 'Metacello-Gitlab-Tests');
			group: 'default' with: #('Core') ]
]
