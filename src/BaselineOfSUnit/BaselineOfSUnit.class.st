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
BaselineOfSUnit class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baselines' }
BaselineOfSUnit >> baseline: spec [
	<baseline>
	spec for: #common
		do: [ 
			spec baseline: 'Traits' with: [
				spec loads: 'core'. 
				spec repository: (self packageRepositoryURLForSpec: spec) ].
			
			spec baseline: 'Debugging' with: [ 
				spec loads: 'Core' from: (self packageRepositoryURLForSpec: spec) ].


			"Package definitions"							
			spec
				package: 'SUnit-Core';
				package: 'SUnit-Core-Traits' with: [ spec requires: 'Traits' ];
				package: 'SUnit-Tests';
				package: 'SUnit-Visitor' with: [ spec requires: #('SUnit-Core') ];
				package: 'SUnit-Visitor-Tests' with: [ spec requires: #('SUnit-Visitor') ];
				package: 'SUnit-MockObjects' with: [ spec requires: #('SUnit-Core') ];
				package: 'SUnit-MockObjects-Tests' with: [ spec requires: #('SUnit-MockObjects') ];
				package: 'SUnit-UI' with: [ spec requires: #('Coverage' 'SUnit-Basic-CLI') ];
				package: 'Coverage';
				package: 'Coverage-Tests' with: [ spec requires: #('Coverage') ];				
				package: 'SUnit-Basic-CLI' with: [ spec requires: #('SUnit-Core' 'Debugging') ];
				package: 'SUnit-UI-Tests' with: [ spec requires: #('SUnit-UI') ].

			"Custom groups"				
			spec
				group: 'Core' with: #('SUnit-Core' 'SUnit-Core-Traits' 'SUnit-Visitor' 'SUnit-MockObjects' 'SUnit-Basic-CLI');
				group: 'UI' with: #('SUnit-UI' 'Coverage');
				group: 'Tests' with: #('SUnit-Tests' 'SUnit-Visitor-Tests' 'SUnit-MockObjects-Tests' 'Coverage-Tests' 'SUnit-UI-Tests').

			"Virtual groups"	
			spec
				group: 'all' with: #('Core' 'UI' 'Tests');
				group: 'default' with: #('Core') ]
]
