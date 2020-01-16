"
Metacello new
  baseline: 'EnlumineurFormatter';
  repository: 'github://Ducasse/enlumineur/src';
  load: 'Tests'.
"
Class {
	#name : #BaselineOfEnlumineurFormatter,
	#superclass : #BaselineOf,
	#category : #BaselineOfEnlumineurFormatter
}

{ #category : #baselines }
BaselineOfEnlumineurFormatter >> baseline: spec [
	<baseline>

	spec for: #pharo do: [
		spec 
			package: 'EnlumineurFormatter';
			package: 'EnlumineurFormatter-Tests' with: [ spec requires: #('EnlumineurFormatter') ];
			package: 'EnlumineurFormatterUI' with: [ spec requires: #('EnlumineurFormatter') ];
			package: 'EnlumineurFormatterUI-Tests' with: [ spec requires: #('EnlumineurFormatterUI') ]. 
		
		spec
			group: 'Core' with: #('EnlumineurFormatter');
			group: 'Tests' with: #('Core' 'EnlumineurFormatter-Tests');
			group: 'UI' with: #('Core' 'EnlumineurFormatterUI');
			group: 'UI-Tests' with: #('Tests' 'EnlumineurFormatterUI-Tests');
			group: 'All' with: #('Core' 'Tests' 'UI' 'UI-Tests')
		
		]
]
