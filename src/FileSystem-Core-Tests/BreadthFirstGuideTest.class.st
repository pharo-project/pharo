"
SUnit tests for class BreadthFirstGuide
"
Class {
	#name : 'BreadthFirstGuideTest',
	#superclass : 'GuideTest',
	#category : 'FileSystem-Core-Tests-Guide',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Guide'
}

{ #category : 'tests' }
BreadthFirstGuideTest >> testAll [
	guide := BreadthFirstGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'epsilon'
			'delta'
			'gamma'
			'zeta'
		)
]
