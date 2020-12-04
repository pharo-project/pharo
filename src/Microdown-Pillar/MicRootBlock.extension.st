Extension { #name : #MicRootBlock }

{ #category : #'*Microdown-Pillar' }
MicRootBlock >> asPillar [
	^ PRDocument new
		setChildren: (self children collect: [ :child | child asPillar ]);
		yourself
]
