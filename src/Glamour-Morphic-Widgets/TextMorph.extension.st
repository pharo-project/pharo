Extension { #name : #TextMorph }

{ #category : #'*glamour-morphic-widgets' }
TextMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]
