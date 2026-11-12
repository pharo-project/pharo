Extension { #name : 'MetacelloBaselineConstructor' }

{ #category : '*Metacello-MVS' }
MetacelloBaselineConstructor >> pharo: anInteger [
	"The minimum Pharo major this baseline needs. Read by mvs before loading (an older image is an error); ignored by a plain load"

	
]

{ #category : '*Metacello-MVS' }
MetacelloBaselineConstructor >> replace: aProjectName with: aRepositoryString [
	"With mvs, and only when this baseline is the root: use aRepositoryString (an exact version) for aProjectName wherever it is required. Ignored by a plain load"

	
]
