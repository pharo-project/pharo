Class {
	#name : #GLMWrapperTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMWrapperTest >> testDefaultPane [
	| browser |
	browser := GLMWrapper new.
	self assert: browser panes size equals: 1
]

{ #category : #tests }
GLMWrapperTest >> testPresentationsAreInstalled [
	| browser |
	browser := GLMWrapper new.
	browser show: [ :a | a text ].
	browser startOn: 42.
	self assert: browser panes anyOne presentations size equals: 1.
	self assert: (browser panes anyOne presentations first isKindOf: GLMRubricTextPresentation)
]
