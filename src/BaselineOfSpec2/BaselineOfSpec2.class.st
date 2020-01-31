Class {
	#name : #BaselineOfSpec2,
	#superclass : #BaselineOf,
	#category : #BaselineOfSpec2
}

{ #category : #baseline }
BaselineOfSpec2 >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ self commander2: spec.

			"Packages"
			spec
				package: 'Spec2-Core' with: [ spec requires: #('Spec2-Layout' 'Spec2-Transmission' 'Spec2-Commands') ];
				package: 'Spec2-CommandLine' with: [ spec requires: #('Spec2-Core') ];
				"package: 'Spec2-Help';"
				package: 'Spec2-Commands' with: [ spec requires: #('Commander2') ];
				package: 'Spec2-Deprecated' with: [ spec requires: #('Spec2-Tests' 'Spec2-Adapters-Morphic' 'Spec2-Commander2') ];
				package: 'Spec2-ObservableSlot';
				package: 'Spec2-Layout' with: [ spec requires: #('Spec2-ObservableSlot') ];
				package: 'Spec2-Transmission';
				package: 'Spec2-Adapters-Morphic' with: [ spec requires: #('Spec2-Core') ];
				package: 'Spec2-Adapters-Stub' with: [ spec requires: #('Spec2-Core') ];
				package: 'Spec2-Examples';
				package: 'Spec2-Interactions' with: [ spec requires: #('Spec2-Core') ];
				package: 'Spec2-Commander2' with: [ spec requires: #('Spec2-Core' 'Spec2-Interactions' 'Commander2') ];
				package: 'Spec2-Commander2-Tests' with: [ spec requires: #('Spec2-Commander2') ];
				package: 'Spec2-Commander2-ContactBook' with: [ spec requires: #('Spec2-Commander2') ];
				package: 'Spec2-Commander2-ContactBook-Extensions' with: [ spec requires: #('Spec2-Commander2-ContactBook') ];
				package: 'Spec2-Tests' with: [ spec requires: #('Spec2-Examples') ];
				package: 'Spec2-Morphic-Backend-Tests' with: [ spec requires: #('Spec2-Adapters-Morphic') ];
				package: 'Spec2-Backend-Tests' with: [ spec requires: #('Spec2-Adapters-Morphic') ];
				package: 'Spec2-Adapters-Morphic-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Backend-Tests') ]].

	spec
		for: #'pharo7.x'
		do: [ "Dependencies for Pharo7"
			self parametrizedTests: spec.

			spec
				baseline: 'Commander'
				with: [ spec
						repository: 'github://pharo-ide/Commander:v0.8.1/src';
						loads: #('Commander-Spec2-Compatibility') ].

			spec
				package: 'Spec2-Pharo7To8Compatibility';
				package: 'Spec2-Backend-Tests' with: [ spec requires: #('ParametrizedTests' 'Spec2-Adapters-Morphic') ];
				package: 'Spec2-Morphic-Backend-Tests' with: [ spec requires: #('ParametrizedTests' 'Spec2-Adapters-Morphic') ];
				package: 'Spec2-Tests' with: [ spec requires: #('Spec2-Examples' 'ParametrizedTests') ] ].
	spec
		for: #'pharo8.x'
		do: [ spec
				package: 'Spec2-Tools' with: [ spec requires: #('Spec2-Core') ];
				package: 'Spec2-Tools-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Tools') ];
				package: 'Spec2-Examples' with: [ spec requires: #('Spec2-Tools') ];
				package: 'Spec2-Deprecated-Tools' with: [ spec requires: #('Spec2-Deprecated' 'Spec2-Tools') ] ]
]

{ #category : #dependencies }
BaselineOfSpec2 >> commander2: spec [
	spec
		baseline: 'Commander2'
		with: [ spec repository: 'github://pharo-spec/Commander2:v2.1.x/src' ]
]

{ #category : #dependencies }
BaselineOfSpec2 >> parametrizedTests: spec [
	spec baseline: 'ParametrizedTests' with: [ spec repository: 'github://tesonep/ParametrizedTests/src' ]
]

{ #category : #accessing }
BaselineOfSpec2 >> project [
	"Atomic loading is needed for Spec because we are moving classes of package and it breaks their subclasses. 
	
	Here is an explanation I wrote on Discord:
	
«The problem with Obsolete classes is that classes with subclasses were moved from Spec-Core to Spec-Deprecated80. 

When updating Spec it will first load Spec-Core. This will delete the class and its subclasses will be migrated to an obsolete superclass. 
Then Spec-Deprecated80 is loaded but it does not migrate the subclasses of the obsolete class to it. (Apparently there is an intern working on that Pablo told me).»"

	^ super project
		loadType: #atomic;
		yourself
]
