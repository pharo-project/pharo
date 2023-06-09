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

{ #category : #adding }
BaselineOfFuel >> addPostloadHacks [
	"Simply perform #addHacks again. We've now loaded classes that weren't
	there before and some of those also need extensions" 
	(Smalltalk at: #FLPlatform) current addHacks
]

{ #category : #baselines }
BaselineOfFuel >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURL.
	spec for: #common do: [
		spec postLoadDoIt: #addPostloadHacks.

		spec baseline: 'FuelPlatform' with: [ spec repository: repository ].

		spec
			package: 'Fuel-Core' with: [ spec requires: 'FuelPlatform' ];
			package: 'Fuel-Tests-Core' with: [ spec requires: 'Core' ].

		spec
			group: 'Core' with: #( 'Fuel-Core' );
			group: 'Tests' with: #( 'Core' 'Fuel-Tests-Core' ) ]
]
