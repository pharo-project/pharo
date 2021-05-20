Class {
	#name : #SpDrawStyleTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpDrawStyleTest >> testToColor [
	| style |
	
	style := SpDrawStyle new.
	self assert: (style toColor: #FF0000) equals: Color red.
	self assert: (style toColor: #'00FF00FF') equals: Color green.
	self assert: (style toColor: #blue) equals: Color blue
]
