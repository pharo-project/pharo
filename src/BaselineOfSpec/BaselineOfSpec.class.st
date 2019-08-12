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
		do: [ spec baseline: 'Morphic' with: [ spec repository: self packageRepositoryURL ].
			spec package: 'Spec-Core'.
			spec package: 'Spec-Layout'.
			spec package: 'Spec-MorphicAdapters'.
			spec package: 'Spec-PolyWidgets'.
			spec package: 'Spec-StubAdapter'.
			spec package: 'Spec-Examples'.
			spec package: 'Spec-Tests'.
			spec package: 'Spec-Deprecated' ]
]
