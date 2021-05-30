Class {
	#name : #NullWorldRendererTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests'
}

{ #category : #tests }
NullWorldRendererTest >> testConvertWindowMouseEventPosition [
	| renderer |
	

	renderer := NullWorldRenderer new.
	self 
		assert: (renderer convertWindowMouseEventPosition: 0@0)
		equals: 0@0
]
