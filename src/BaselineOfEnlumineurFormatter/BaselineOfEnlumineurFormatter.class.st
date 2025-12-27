"
Metacello new
  baseline: 'EnlumineurFormatter';
  repository: 'github://Ducasse/enlumineur/src';
  load: 'Tests'.
"
Class {
	#name : 'BaselineOfEnlumineurFormatter',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfEnlumineurFormatter',
	#package : 'BaselineOfEnlumineurFormatter'
}

{ #category : 'baselines' }
BaselineOfEnlumineurFormatter >> baseline: spec [

	<baseline>
	spec for: #pharo do: [
			spec
				package: 'EnlumineurFormatter';
				package: 'EnlumineurFormatter-Tests' with: [ spec requires: #( 'EnlumineurFormatter' ) ].

			spec
				group: 'Core' with: #( 'EnlumineurFormatter' );
				group: 'Tests' with: #( 'Core' 'EnlumineurFormatter-Tests' );
				group: 'All' with: #( 'Core' 'Tests' ) ]
]
