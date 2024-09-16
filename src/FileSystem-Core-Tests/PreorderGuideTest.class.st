"
SUnit tests for class PreorderGuide
"
Class {
	#name : 'PreorderGuideTest',
	#superclass : 'GuideTest',
	#category : 'FileSystem-Core-Tests-Guide',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Guide'
}

{ #category : 'tests' }
PreorderGuideTest >> testAll [
	guide := PreorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'delta'
			'gamma'
			'epsilon'
			'zeta'
		)
]
