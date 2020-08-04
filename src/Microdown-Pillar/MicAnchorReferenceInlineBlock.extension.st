Extension { #name : #MicAnchorReferenceInlineBlock }

{ #category : #'*Microdown-Pillar' }
MicAnchorReferenceInlineBlock >> asPillar [

	^ PRInternalLink new anchor: substring; yourself
]

{ #category : #'*Microdown-Pillar' }
MicAnchorReferenceInlineBlock >> associatedPillarClass [
	^ PRInternalLink
]
