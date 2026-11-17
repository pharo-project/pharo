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
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec
			baseline: 'Display' with: [ spec repository: repository ];
			baseline: 'Clap' with: [ spec loads: #( 'Tests' ) from: repository ].

		spec
			package: 'Fuel-Core' with: [ spec requires: #( 'Display' ) ];
			package: 'Fuel-Core-Tests' with: [ spec requires: #( 'Core' 'Clap' ) ].

		spec
			group: 'Core' with: #( 'Fuel-Core' );
			group: 'Tests' with: #( 'Core' 'Fuel-Core-Tests' ) ]

]
