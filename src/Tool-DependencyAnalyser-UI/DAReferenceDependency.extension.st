Extension { #name : #DAReferenceDependency }

{ #category : #'*Tool-DependencyAnalyser-UI' }
DAReferenceDependency >> browseIn: aBrowser [

	aBrowser selectMethod: self method
]

{ #category : #'*Tool-DependencyAnalyser-UI' }
DAReferenceDependency >> nodeClass [
	^ DAReferenceDependencyNode
]
