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
  spec
	for: #common
	do: [
		spec postLoadDoIt: #addPostloadHacks.

		self   
			fuelPlatform: spec;
			smark: spec.

		spec
			package: 'Fuel-Core' with: [ spec requires: 'FuelPlatform' ];
			package: 'Fuel-Debug' with: [ spec requires: 'Core' ];
			package: 'Fuel-Benchmarks' with: [ spec requires: #('Core' 'SMark-Jenkins') ];
			
			package: 'Fuel-Tests-Core' with: [ spec requires: 'Core' ];
			package: 'Fuel-Tests-Debug' with: [ spec requires: #('Fuel-Debug' 'Tests') ];
			package: 'Fuel-Tests-Benchmarks' with: [ spec requires: #('Fuel-Benchmarks' 'Tests') ].
					
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('Fuel-Core');
			group: 'Tests' with: #('Core' 'Fuel-Tests-Core');
			group: 'Debug' with: #('Core' 'Fuel-Debug') ].

	self
		groupsForPharoCommon: spec;
		groupsForSqueakCommon: spec
]

{ #category : #'baselines-helpers' }
BaselineOfFuel >> fuelPlatform: spec [		
	spec 
		baseline: 'FuelPlatform' 
		with: [ 
			spec
				repository: 'github://theseion/FuelPlatform:v5/repository';
  				loads: 'default' ]
]

{ #category : #'baselines-helpers' }
BaselineOfFuel >> groupsForPharoCommon: spec [
	spec for: #pharo do: [
		spec
			group: 'DevelopmentGroup' with: #('Debug' 'Tests' 'Fuel-Tests-Debug');
			group: 'Benchmarks' with: #('Core' 'Fuel-Benchmarks' 'Fuel-Tests-Benchmarks') ]
]

{ #category : #'baselines-helpers' }
BaselineOfFuel >> groupsForSqueakCommon: spec [
	spec for: #squeak do: [
		spec
			group: 'Benchmarks'
			with: #('Core' 'Fuel-Benchmarks') ]
]

{ #category : #'baselines-helpers' }
BaselineOfFuel >> smark: spec [		
	spec
		project: 'SMark' with: [
			spec
				className: 'ConfigurationOfSMark';
				versionString: #development;
				file: 'ConfigurationOfSMark';
				repository: 'http://smalltalkhub.com/mc/StefanMarr/SMark/main' ].

	spec
		package: 'SMark-Jenkins' with: [ 
			spec 
				repository: 'http://smalltalkhub.com/mc/ClementBera/classic-bench/main';
				requires: 'SMark' ]

]
