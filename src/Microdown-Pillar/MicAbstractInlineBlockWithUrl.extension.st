Extension { #name : #MicAbstractInlineBlockWithUrl }

{ #category : #'*Microdown-Pillar' }
MicAbstractInlineBlockWithUrl >> asPillar [
	^ self class associatedPillarClass new
		setChildren: ((MicInlineSplitter new start: self substring) collect: [:n | n asPillar]);
		reference: self url;
		yourself
]

{ #category : #'*Microdown-Pillar' }
MicAbstractInlineBlockWithUrl class >> associatedPillarClass [
	"I am abstract"
]
