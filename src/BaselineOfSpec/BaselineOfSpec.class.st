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
       
	spec for: #common do: [
		spec baseline: 'Morphic' with: [spec repository: repository].
		spec package: 'Spec-Core'.
		spec package: 'Spec-Inspector'.
		spec package: 'Spec-Layout'.
		spec package: 'Spec-MorphicAdapters'.
		spec package: 'Spec-PolyWidgets'.
		spec package: 'Spec-Tools'.
		spec package: 'Spec-StubAdapter'.
		spec package: 'Spec-Deprecated'.
	].
]

{ #category : #baseline }
BaselineOfSpec >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
	
]
