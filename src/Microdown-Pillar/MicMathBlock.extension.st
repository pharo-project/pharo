Extension { #name : #MicMathBlock }

{ #category : #'*Microdown-Pillar' }
MicMathBlock >> asPillar [ 
	^ PRMathEnvironment new 
		add: (PRText content: self body);
		yourself
	
]
