"
I load all of Morphic: the widgets (`BaselineOfMorphicWidgets`) and the desktop built on them (`BaselineOfMorphicDesktop`).

The bootstrap and the IDE load me. To load only part of Morphic, load one of those two baselines instead.
"
Class {
	#name : 'BaselineOfMorphic',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMorphic',
	#package : 'BaselineOfMorphic'
}

{ #category : 'baseline' }
BaselineOfMorphic >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
		spec baseline: 'MorphicWidgets' with: [ spec repository: repository ].
		spec baseline: 'MorphicDesktop' with: [ spec repository: repository ] ]
]

{ #category : 'baseline' }
BaselineOfMorphic >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
]
