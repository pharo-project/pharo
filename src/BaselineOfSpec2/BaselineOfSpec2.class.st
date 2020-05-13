Class {
	#name : #BaselineOfSpec2,
	#superclass : #BaselineOf,
	#category : #BaselineOfSpec2
}

{ #category : #baseline }
BaselineOfSpec2 >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		self commander2: spec.
		self pillar: spec.
		
		spec
			package: 'Spec2-ObservableSlot';
			package: 'Spec2-Core' with: [ 
				spec requires: #(
					'Spec2-ObservableSlot'
					'Spec2-Layout' 
					'Spec2-Commands') ];
			package: 'Spec2-CommandLine' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Commands' with: [ spec requires: #('Commander2') ];
			package: 'Spec2-Layout' with: [ spec requires: #('Spec2-ObservableSlot') ];
			package: 'Spec2-Transmission' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Code' with: [ 
				spec requires: #(
					'Spec2-Core' 
					'Spec2-Commands') ];
			package: 'Spec2-Code-Tests' with: [ spec requires: #('Spec2-Code') ];
			package: 'Spec2-Adapters-Morphic' with: [ spec requires: #( 'Spec2-Core' ) ];
			package: 'Spec2-Code-Morphic' with: [ 
				spec requires: #(
					'Spec2-Code' 
					'Spec2-Adapters-Morphic') ];
			package: 'Spec2-Adapters-Stub' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Interactions' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Commander2' with: [ 
				spec requires: #(
					'Spec2-Core' 
					'Spec2-Interactions' 
					'Commander2') ];
			package: 'Spec2-Commander2-Tests' with: [ spec requires: #('Spec2-Commander2') ];
			package: 'Spec2-Commander2-ContactBook' with: [ spec requires: #('Spec2-Commander2') ];
			package: 'Spec2-Commander2-ContactBook-Extensions' with: [ spec requires: #('Spec2-Commander2-ContactBook') ];
			package: 'Spec2-Tests' with: [ spec requires: #('Spec2-Examples') ];
			package: 'Spec2-Backend-Tests' with: [ spec requires: #('Spec2-Adapters-Morphic') ];
			package: 'Spec2-Morphic-Backend-Tests' with: [ spec requires: #('Spec2-Adapters-Morphic' 'Spec2-Backend-Tests') ];
			package: 'Spec2-Adapters-Morphic-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Backend-Tests') ];
			package: 'Spec2-Tools' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Tools-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Tools') ];
			package: 'Spec2-Morphic' with: [ spec requires: #('Spec2-Adapters-Morphic') ];
			package: 'Spec2-Morphic-Tests' with: [ spec requires: #('Spec2-Morphic' 'Spec2-Backend-Tests') ];
			package: 'Spec2-Examples' with: [ spec requires: #('Spec2-Morphic') ];
			package: 'Spec2-Pillar' with: [ spec requires: #('Spec2-Core' 'PillarCore') ].
			"This will extend the help tool, which in boostrap will be loaded a lot later."
			"package: 'Spec2-Help';"		
		 ].
	
	spec group: 'Core' with: #(
		'Spec2-ObservableSlot' 
		'Spec2-Layout' 
		'Spec2-Transmission'
		'Spec2-Commands' 
		'Spec2-Core' 
		'Spec2-CommandLine' 
		"'Spec2-Code'" 
		'Spec2-Adapters-Stub' 
		'Spec2-Interactions' 
		'Spec2-Commander2' ).
	spec group: 'Code' with: #(
		'Spec2-Code'
		'Spec2-Code-Morphic'
		'Spec2-Code-Tests').
	spec group: 'Morphic' with: #(
		'Core' 
		'Spec2-Morphic'
		'Spec2-Adapters-Morphic' ).
	spec group: 'MorphicSupport' with: #( 
		'Core'
		'Morphic'
		'Spec2-Examples'
		'Spec2-Commander2-ContactBook' 
		'Spec2-Commander2-ContactBook-Extensions'  
		'Spec2-Tools' ).
	spec group: 'Tests' with: #( 
		'Core' 
		'Spec2-Tests' 
		'Spec2-Commander2-Tests').
	spec group: 'MorphicSupportTests' with: #( 
		'MorphicSupport'  
		'Spec2-Tools-Tests' 
		'Spec2-Adapters-Morphic-Tests' 
		'Spec2-Morphic-Tests' 
		'Spec2-Morphic-Backend-Tests' 
		'Spec2-Backend-Tests' ).
	spec group: 'Pillar' with: #(
		'Spec2-Pillar' ).
	spec group: 'Base' with: #(
		'Core'
		'Morphic' 
		'MorphicSupport'
		'Spec2-Tests'
		'Spec2-Morphic-Tests'
		'MorphicSupportTests' ).
	spec group: 'default' with: #('Base' 'Code').

	spec for: #'pharo8.x' do: [
		self complishon: spec.

		spec
			package: 'Spec2-Code' with: [ 
				spec requires: #(
					'Spec2-Core' 
					'Spec2-Commands'
					'HeuristicCompletion') ];
			package: 'Spec2-Tools' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Tools-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Tools') ];
			package: 'Spec2-Examples' with: [ spec requires: #('Spec2-Tools') ] ].
]

{ #category : #dependencies }
BaselineOfSpec2 >> commander2: spec [
	spec
		baseline: 'Commander2'
		with: [ spec repository: 'github://pharo-spec/Commander2:v2.1.x/src'; loads: #('core' 'ui') ]
]

{ #category : #dependencies }
BaselineOfSpec2 >> complishon: spec [
	
	spec
		baseline: 'HeuristicCompletion'
		with: [ spec repository: 'github://guillep/complishon' ]
]

{ #category : #dependencies }
BaselineOfSpec2 >> pillar: spec [
	spec
		baseline: 'PillarCore'
		with: [ spec repository: 'github://pillar-markup/pillar:v7.7.5/src' ]
]

{ #category : #acccessing }
BaselineOfSpec2 >> project [
	"Atomic loading is needed for Spec because we are moving classes of package and it breaks their subclasses. 
	
	Here is an explanation I wrote on Discord:
	
«The problem with Obsolete classes is that classes with subclasses were moved from Spec-Core to Spec-Deprecated80. 

When updating Spec it will first load Spec-Core. This will delete the class and its subclasses will be migrated to an obsolete superclass. 
Then Spec-Deprecated80 is loaded but it does not migrate the subclasses of the obsolete class to it. (Apparently there is an intern working on that Pablo told me).»

We need to use atomic loading only during update of existing Spec upgrade"

	^self class environment at: #SpPresenter 
		ifPresent: [
			super project
				loadType: #atomic;
				yourself ] 
		ifAbsent: [ 		
			super project
				loadType: #linear;
				yourself  ]

	 
]
