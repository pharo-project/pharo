Class {
	#name : #SpStylePropertyTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpStylePropertyTest >> testMergeWith [
	| p1 p2 p3 |
	
	p1 := SpStyleGeometry new
		extent: 20@20;
		yourself.
	p2 := SpStyleGeometry new
		extent: 40@40;
		yourself.
		
	p3 := p1, p2.
	
	self assert: p3 extent equals: 40@40.
	
]
