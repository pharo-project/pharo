Extension { #name : 'Package' }

{ #category : '*Tool-DependencyAnalyser' }
Package >> ignoredDependencies [

	^ self packageManifestOrNil
		ifNil: [ #() ]
		ifNotNil: #ignoredDependencies
]

{ #category : '*Tool-DependencyAnalyser' }
Package >> manuallyResolvedDependencies [

	^ self packageManifestOrNil
		ifNil: [ #() ]
		ifNotNil: #manuallyResolvedDependencies
]
