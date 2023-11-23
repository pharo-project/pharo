Extension { #name : 'DATraitDependency' }

{ #category : '*Tool-DependencyAnalyser-UI' }
DATraitDependency >> browseIn: aSystemBrowser [

	aSystemBrowser selectClass: self theClass
]

{ #category : '*Tool-DependencyAnalyser-UI' }
DATraitDependency >> nodeClass [
	^ DATraitDependencyNode
]
