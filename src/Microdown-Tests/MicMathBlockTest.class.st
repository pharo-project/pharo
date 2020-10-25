Class {
	#name : #MicMathBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests'
}

{ #category : #tests }
MicMathBlockTest >> testMathBlockIsMathBlock [

	| root |
	root := (MicroDownParser new parse: '

&
V_i = C_0 - C_3
&


').
	self assert: root children first class equals: MicMathBlock 
]
