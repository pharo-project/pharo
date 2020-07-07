Extension { #name : #MicParagraphBlock }

{ #category : #'*Microdown-Pillar' }
MicParagraphBlock >> asPillar [
	^ PRParagraph  new
		setChildren: (self pillarFromString: text);
		yourself
]
