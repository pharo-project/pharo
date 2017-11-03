"
This baseline is used to generate Pharo images from the local clone of pharo-core repository. 

Depends on:
- minimal Pharo image

Load type: 
- linear

Installs:
- SUnit without GUI
"
Class {
	#name : #BaselineOfSUnit,
	#superclass : #BaselineOf,
	#category : #BaselineOfSUnit
}

{ #category : #baseline }
BaselineOfSUnit >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec 
			package: 'SUnit-Core';
			package: 'SUnit-Tests';
			package: 'SUnit-UI';
			package: 'SUnit-UITesting';
			package: 'SUnit-Help';
			package: 'JenkinsTools-Core';
			package: 'JenkinsTools-ExtraReports'.
		spec 
			group: 'Core' with: #('SUnit-Core');
			group: 'UI' with: #('SUnit-UI' 'SUnit-UITesting' );
			group: 'Tests' with: #('SUnit-Tests' );
			group: 'Help' with: #('SUnit-Help' );
			group: 'JenkinsSupport' with: #('JenkinsTools-Core' 'JenkinsTools-ExtraReports');
			group: 'default' with: #('SUnit-Core' 'SUnit-Tests' 'JenkinsSupport' ). ].		

]
