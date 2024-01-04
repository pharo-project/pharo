"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- minimal Pharo image

Load type: 
- linear

Installs:
- SUnit without GUI
"
Class {
	#name : 'BaselineOfSUnit',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSUnit',
	#package : 'BaselineOfSUnit'
}

{ #category : 'accessing' }
BaselineOfSUnit class >> defaultPackageNames [

	^ self packagesOfGroupNamed: #default
]

{ #category : 'baselines' }
BaselineOfSUnit >> baseline: spec [
	<baseline>
	spec for: #common
		do: [ 
	
			spec baseline: 'Traits' with: [
				spec loads: 'core'. 
				spec repository: (self packageRepositoryURLForSpec: spec)
			].
			
			self teachable: spec.
			
			spec
				package: 'SUnit-Core';
				package: 'SUnit-Core-Traits' with: [ spec requires: 'Traits' ];
				package: 'SUnit-Tests';
				package: 'SUnit-Visitor' with: [ spec requires: #('SUnit-Core') ];
				package: 'SUnit-Visitor-Tests' with: [ spec requires: #('SUnit-Visitor') ];
				package: 'SUnit-MockObjects' with: [ spec requires: #('Teachable' 'SUnit-Core') ];
				package: 'SUnit-MockObjects-Tests' with: [ spec requires: #('Teachable-Tests' 'SUnit-MockObjects') ];
				package: 'SUnit-UI' with: [ spec requires: #('Coverage') ];
				package: 'SUnit-Support-UITesting-Tests';
				package: 'SUnit-Support-UITesting';
				package: 'Coverage';
				package: 'JenkinsTools-Core';
				package: 'JenkinsTools-ExtraReports'.
			spec
				group: 'Core' with: #('SUnit-Core' 'SUnit-Core-Traits' 'SUnit-Visitor');
				group: 'MockObjects' with: #('SUnit-MockObjects');
				group: 'UI' with: #('SUnit-UI' 'SUnit-Support-UITesting' 'Coverage');
				group: 'Tests' with: #('SUnit-Tests' 'SUnit-Support-UITesting-Tests' 'SUnit-Visitor-Tests' 'SUnit-MockObjects-Tests');
				group: 'JenkinsSupport' with: #('JenkinsTools-Core' 'JenkinsTools-ExtraReports');
				group: 'default' with: #('SUnit-Core' 'SUnit-Core-Traits' 'SUnit-Tests' 'JenkinsTools-Core' 'JenkinsTools-ExtraReports' 'SUnit-MockObjects') ]
]

{ #category : 'dependencies' }
BaselineOfSUnit >> teachable: spec [

	spec baseline: 'Teachable' with: [
		spec
			repository: 'github://astares/Pharo-Teachable:v2.3/src';
			loads: #( 'Core' ) ].

	spec
		project: 'Teachable-Tests'
		copyFrom: 'Teachable'
		with: [ spec loads: #( 'Tests' ) ]
]
