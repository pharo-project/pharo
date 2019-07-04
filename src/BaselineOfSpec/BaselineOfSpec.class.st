Class {
	#name : #BaselineOfSpec,
	#superclass : #BaselineOf,
	#category : #BaselineOfSpec
}

{ #category : #baseline }
BaselineOfSpec >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [

			spec
				package: 'Spec-Core' with: [ spec requires: #('Spec-Layout') ];
				package: 'Spec-Deprecated80' with: [ spec requires: #('Spec-Tests' 'Spec-PolyWidgets-Tests' 'Spec-MorphicAdapters') ];
				package: 'Spec-Deprecated' with: [ spec requires: #('Spec-Core' 'Spec-Deprecated80' 'Spec-PolyWidgets' 'Spec-MorphicAdapters') ];
				
				package: 'Spec-Layout';
				package: 'Spec-Transmission';
				package: 'Spec-MorphicAdapters' with: [ spec requires: #('Spec-Core') ];
				package: 'Spec-PolyWidgets' with: [ spec requires: #('Spec-Core' 'Spec-Transmission') ];
				package: 'Spec-StubAdapter' with: [ spec requires: #('Spec-Core') ];
				package: 'Spec-Examples';
				package: 'Spec-Tests' with: [ spec requires: #('Spec-PolyWidgets' 'Spec-Examples') ];
				package: 'Spec-PolyWidgets-Tests' with: [ spec requires: #('Spec-Tests' 'Spec-PolyWidgets') ];
				package: 'Spec-MorphicBackendTests' with: [ spec requires: #('Spec-MorphicAdapters' 'Spec-BackendTests') ];
				package: 'Spec-BackendTests' with: [ spec requires: #('Spec-MorphicAdapters') ];
				package: 'Spec-CompatibilityTests' with: [ spec requires: #('Spec-MorphicBackendTests' 'Spec-Deprecated80') ] ]
]
