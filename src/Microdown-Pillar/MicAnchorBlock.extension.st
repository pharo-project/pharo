Extension { #name : #MicAnchorBlock }

{ #category : #'*Microdown-Pillar' }
MicAnchorBlock >> asPillar [ 
	^ PRAnchor new name: self label; yourself
]
