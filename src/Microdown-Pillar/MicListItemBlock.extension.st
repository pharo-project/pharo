Extension { #name : #MicListItemBlock }

{ #category : #'*Microdown-Pillar' }
MicListItemBlock >> asPillar [
	^ PRListItem new
		setChildren: (self pillarFromString: text), (self children collect: [:child| child asPillar]);
		yourself
]
