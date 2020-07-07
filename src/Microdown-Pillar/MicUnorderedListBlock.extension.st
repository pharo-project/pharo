Extension { #name : #MicUnorderedListBlock }

{ #category : #'*Microdown-Pillar' }
MicUnorderedListBlock >> asPillar [
	^ PRUnorderedList new
		setChildren: (self children collect:[:child| child asPillar]);
		yourself
]
