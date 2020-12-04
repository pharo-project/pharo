Class {
	#name : #SpStyleDrawTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpStyleDrawTest >> testToColor [
	| style |
	
	style := SpStyleDraw new.
	self assert: (style toColor: #FF0000) equals: Color red.
	self assert: (style toColor: #'00FF00FF') equals: Color green.
	self assert: (style toColor: #blue) equals: Color blue
]
