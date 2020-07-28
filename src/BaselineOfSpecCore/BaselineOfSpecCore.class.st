Class {
	#name : #BaselineOfSpecCore,
	#superclass : #BaselineOf,
	#category : #BaselineOfSpecCore
}

{ #category : #baseline }
BaselineOfSpecCore >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		self commander2: spec.
		self pillar: spec.
		
		spec
			"Core"
			package: 'Spec2-ObservableSlot';
			package: 'Spec2-Layout' with: [ spec requires: #('Spec2-ObservableSlot') ];
			package: 'Spec2-Core' with: [ spec requires: #('Spec2-ObservableSlot' 'Spec2-Layout') ];
			package: 'Spec2-CommandLine' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Commands' with: [ spec requires: #('Commander2') ];
			package: 'Spec2-Transmission' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Interactions' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Commander2' with: [ spec requires: #('Spec2-Core' 'Spec2-Interactions' 'Commander2') ];
			"Code"
			package: 'Spec2-Code' with: [ spec requires: #('Spec2-Core' 'Spec2-Commands') ];
			package: 'Spec2-Code-Commands' with: [ spec requires: #('Spec2-Core' 'Spec2-Commands') ];
			"Tests"			
			package: 'Spec2-Adapters-Stub' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Commander2-Tests' with: [ spec requires: #('Spec2-Commander2') ];
			package: 'Spec2-Tests' with: [ spec requires: #('Spec2-Examples') ];
			package: 'Spec2-Code-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Code') ];
			"Tools -> This needs to be moved to NewTools"
			package: 'Spec2-Tools' with: [ spec requires: #('Spec2-Core') ];
			package: 'Spec2-Tools-Tests' with: [ spec requires: #('Spec2-Tests' 'Spec2-Tools') ];
			"Examples"
			package: 'Spec2-Examples';
			package: 'Spec2-Commander2-ContactBook' with: [ spec requires: #('Spec2-Commander2') ];
			package: 'Spec2-Commander2-ContactBook-Extensions' with: [ spec requires: #('Spec2-Commander2-ContactBook') ];
			"RichText -> Pillar"
			package: 'Spec2-Pillar' with: [ spec requires: #('Spec2-Core' 'PillarCore') ] 
	].
	
	spec group: 'Core' with: #(
		'Spec2-ObservableSlot' 
		'Spec2-Layout' 
		'Spec2-Transmission'
		'Spec2-Commands' 
		'Spec2-Core' 
		'Spec2-CommandLine' 
		'Spec2-Adapters-Stub' 
		'Spec2-Interactions' 
		'Spec2-Commander2' ).
	spec group: 'Code' with: #('Core' 'Spec2-Code-Commands' 'Spec2-Code').
	spec group: 'CodeTests' with: #('Spec2-Code-Tests').
	spec group: 'Support' with: #('Core' 'Spec2-Examples' 'Spec2-Tools' ).
	spec group: 'Tests' with: #('Core' 'Spec2-Tests' 	'Spec2-Commander2-Tests').
	spec group: 'SupportTests' with: #('Support' 'Spec2-Tools-Tests').
	spec group: 'Pillar' with: #('Spec2-Pillar' ).
	spec group: 'Base' with: #('Core' 'Support').
	spec group: 'BaseTests' with: #('Spec2-Tests' 'SupportTests').
	
	spec group: 'default' with: #('Base' 'BaseTests' 'Code' 'CodeTests').
]

{ #category : #'external projects' }
BaselineOfSpecCore >> commander2: spec [

	spec
		baseline: 'Commander2'
		with: [ 
			spec repository: 'github://pharo-spec/Commander2:v2.1.x/src'"; loads: #('core' 'ui')" ]
]

{ #category : #'external projects' }
BaselineOfSpecCore >> complishon: spec [
	
	spec
		baseline: 'HeuristicCompletion'
		with: [ 
			spec 
				repository: 'github://guillep/complishon';
				loads: 'HeuristicCompletion-Model' ]
]

{ #category : #'external projects' }
BaselineOfSpecCore >> pillar: spec [
	spec
		baseline: 'PillarCore'
		with: [ spec repository: 'github://pillar-markup/pillar:dev-8/src' ]
]

{ #category : #acccessing }
BaselineOfSpecCore >> project [
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
