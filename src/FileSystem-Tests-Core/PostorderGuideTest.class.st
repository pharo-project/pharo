"
SUnit tests for class PostorderGuide
"
Class {
	#name : 'PostorderGuideTest',
	#superclass : 'GuideTest',
	#category : 'FileSystem-Tests-Core-Guide',
	#package : 'FileSystem-Tests-Core',
	#tag : 'Guide'
}

{ #category : 'tests' }
PostorderGuideTest >> testAll [
	guide := PostorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'delta'
			'gamma'
			'beta'
			'zeta'
			'epsilon'
			'alpha'
		)
]
