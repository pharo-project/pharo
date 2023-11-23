Extension { #name : 'DAExtensionDependency' }

{ #category : '*Tool-DependencyAnalyser-UI' }
DAExtensionDependency >> browseIn: aSystemBrowser [

	aSystemBrowser selectMethod: self method
]

{ #category : '*Tool-DependencyAnalyser-UI' }
DAExtensionDependency >> nodeClass [
	^ DAExtensionDependencyNode
]
