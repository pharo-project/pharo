Class {
	#name : #BaselineOfSpec,
	#superclass : #BaselineOf,
	#category : #BaselineOfSpec
}

{ #category : #baseline }
BaselineOfSpec >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.

	spec
		for: #common
		do: [
			spec baseline: 'ParametrizedTests' with: [ spec repository: repository ].

			spec
				package: 'Spec-Core' with: [ spec requires: #('Spec-Layout') ];
				package: 'Spec-Deprecated80' with: [ spec requires: #('Spec-Tests' 'Spec-MorphicAdapters') ];
				package: 'Spec-Deprecated' with: [ spec requires: #('Spec-Core' 'Spec-Deprecated80' 'Spec-PolyWidgets' 'Spec-MorphicAdapters') ];
				
				package: 'Spec-Inspector' with: [ spec requires: #('Spec-PolyWidgets') ];
				package: 'Spec-Layout';
				package: 'Spec-MorphicAdapters' with: [ spec requires: #('Spec-Core') ];
				package: 'Spec-PolyWidgets' with: [ spec requires: #('Spec-Core') ];
				package: 'Spec-Tools' with: [ spec requires: #('Spec-PolyWidgets') ];
				package: 'Spec-StubAdapter' with: [ spec requires: #('Spec-Core') ];
				package: 'Spec-Examples' with: [ spec requires: #('Spec-Tools' 'Spec-Inspector') ];
				package: 'Spec-Tests' with: [ spec requires: #('Spec-PolyWidgets' 'Spec-Examples') ];
				package: 'Spec-MorphicBackendTests' with: [ spec requires: #('Spec-MorphicAdapters' 'ParametrizedTests') ];
				package: 'Spec-BackendTests' with: [ spec requires: #('Spec-MorphicAdapters' 'ParametrizedTests') ];
				package: 'Spec-CompatibilityTests' with: [ spec requires: #('Spec-MorphicBackendTests' 'Spec-Deprecated80') ] ]
]
