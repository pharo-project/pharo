Extension { #name : 'DAPoolDictionaryDependency' }

{ #category : '*Tool-DependencyAnalyser-UI' }
DAPoolDictionaryDependency >> browseIn: aSystemBrowser [

	aSystemBrowser selectClass: self theClass
]

{ #category : '*Tool-DependencyAnalyser-UI' }
DAPoolDictionaryDependency >> nodeClass [
	^ DAPoolDictionaryDependencyNode
]
