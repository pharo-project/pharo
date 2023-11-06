"
SUnit tests for class PreorderGuide
"
Class {
	#name : 'PreorderGuideTest',
	#superclass : 'GuideTest',
	#category : 'FileSystem-Tests-Core-Guide',
	#package : 'FileSystem-Tests-Core',
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
