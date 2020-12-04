Extension { #name : #MicOrderedListBlock }

{ #category : #'*Microdown-Pillar' }
MicOrderedListBlock >> asPillar [
	^ PROrderedList new
		setChildren: (self children collect: [ :child | child asPillar ]);
		counter: self startIndex;
		yourself
]
