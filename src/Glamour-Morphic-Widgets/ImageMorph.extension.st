Extension { #name : #ImageMorph }

{ #category : #'*glamour-morphic-widgets' }
ImageMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]
