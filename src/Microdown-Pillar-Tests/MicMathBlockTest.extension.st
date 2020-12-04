Extension { #name : #MicMathBlockTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicMathBlockTest >> testMathBlockAsPillar [
	| source root pillar |
	source := '
&
V_i = C_0 - C_3
&
	
'.
	root := parser parse: source.
	pillar := root children first asPillar.
	self assert: pillar class equals: PRMathEnvironment.
	self assert: pillar children first class equals: PRText.
	
]
