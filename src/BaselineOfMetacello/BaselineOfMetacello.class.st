Class {
	#name : #BaselineOfMetacello,
	#superclass : #BaselineOf,
	#category : #BaselineOfMetacello
}

{ #category : #baseline }
BaselineOfMetacello >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec 
			package: 'ScriptingExtensions';
			package: 'System-FileRegistry';
			package: 'FileSystem-Memory';
			package: 'Regex-Core';
			package: 'StartupPreferences';
			package: 'ConfigurationCommandLineHandler-Core';
			package: 'PragmaCollector';
			package: 'Reflectivity';
			package: 'System-FileRegistry';
			package: 'MonticelloConfigurations';
			package: 'Gofer-Core';
			package: 'Metacello-Base';
			package: 'Metacello-Core';
			package: 'MonticelloFileTree-Core';
			package: 'Metacello-MC';
			package: 'Metacello-PharoCommonPlatform';
			package: 'MonticelloFileTree-FileSystem-Utilities';
			package: 'STON-Core';
			package: 'Metacello-GitBasedRepository';
			package: 'Metacello-GitHub';
			package: 'Metacello-Gitlab';
			package: 'Metacello-Bitbucket';
			package: 'Metacello-Platform';
			package: 'Metacello-ToolBox';
			package: 'Metacello-PharoExtensions';
			package: 'MetacelloCommandLineHandler-Core'. 

		spec 
			package: 'Metacello-TestsMCResources';
			package: 'Metacello-TestsCore';
			package: 'Metacello-TestsCommonMC';
			package: 'Metacello-TestsMC';	"standalone"
			package: 'Metacello-TestsMCCore';	"standalone"
			package: 'Metacello-TestsPlatform';	"standalone"
			package: 'Metacello-TestsReference';	"standalone"
			package: 'Metacello-Gitlab-Tests'.

		spec 
			group: 'Core' with: #('ScriptingExtensions' 'System-FileRegistry' 'FileSystem-Memory' 'Regex-Core' 'StartupPreferences' 'ConfigurationCommandLineHandler-Core' 'PragmaCollector' 'Reflectivity' 'System-FileRegistry' 'MonticelloConfigurations' 'Gofer-Core' 'Metacello-Base' 'Metacello-Core' 'MonticelloFileTree-Core' 'Metacello-MC' 'Metacello-PharoCommonPlatform' 'MonticelloFileTree-FileSystem-Utilities' 'STON-Core' 'Metacello-GitBasedRepository' 'Metacello-GitHub' 'Metacello-Gitlab' 'Metacello-Bitbucket' 'Metacello-Platform' 'Metacello-ToolBox' 'Metacello-PharoExtensions' 'MetacelloCommandLineHandler-Core');
			group: 'Tests' with: #( 'Metacello-TestsMCResources' 'Metacello-TestsCore' 'Metacello-TestsCommonMC' 'Metacello-TestsMC' 'Metacello-TestsMCCore' 'Metacello-TestsPlatform' 'Metacello-TestsReference' 'Metacello-Gitlab-Tests');
			group: 'default' with: #('Core') ]			
			


]
