Extension { #name : #DAInheritanceDependency }

{ #category : #'*Tool-DependencyAnalyser-UI' }
DAInheritanceDependency >> browseIn: aBrowser [

	aBrowser selectClass: self theClass
]

{ #category : #'*Tool-DependencyAnalyser-UI' }
DAInheritanceDependency >> nodeClass [
	^ DAInheritanceDependencyNode
]
