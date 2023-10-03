"
Fuel is a general-purpose object serialization framework.

See more information in our website:
http://rmod.lille.inria.fr/web/pier/software/Fuel/
"
Class {
	#name : 'BaselineOfFuel',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFuel',
	#package : 'BaselineOfFuel'
}

{ #category : 'baselines' }
BaselineOfFuel >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec
			package: 'Fuel-Core';
			package: 'Fuel-Core-Tests' with: [ spec requires: 'Core' ].

		spec
			group: 'Core' with: #( 'Fuel-Core' );
			group: 'Tests' with: #( 'Core' 'Fuel-Core-Tests' ) ]

]
