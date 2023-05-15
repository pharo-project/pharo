"
Fuel is a general-purpose object serialization framework.

See more information in our website:
http://rmod.lille.inria.fr/web/pier/software/Fuel/
"
Class {
	#name : #BaselineOfFuel,
	#superclass : #BaselineOf,
	#category : #BaselineOfFuel
}

{ #category : #baselines }
BaselineOfFuel >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURL.
	spec for: #common do: [

		spec baseline: 'FuelPlatform' with: [ spec repository: repository ].

		spec
			package: 'Fuel-Core' with: [ spec requires: 'FuelPlatform' ];
			package: 'Fuel-Tests-Core' with: [ spec requires: 'Core' ].

		spec
			group: 'Core' with: #( 'Fuel-Core' );
			group: 'Tests' with: #( 'Core' 'Fuel-Tests-Core' ) ]
]
