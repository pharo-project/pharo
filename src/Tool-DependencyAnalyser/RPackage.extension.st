Extension { #name : #RPackage }

{ #category : #'*Tool-DependencyAnalyser' }
RPackage >> ignoredDependencies [

	^ self packageManifestOrNil
		ifNil: [ #() ]
		ifNotNil: #ignoredDependencies
]

{ #category : #'*Tool-DependencyAnalyser' }
RPackage >> manuallyResolvedDependencies [

	^ self packageManifestOrNil
		ifNil: [ #() ]
		ifNotNil: #manuallyResolvedDependencies
]
