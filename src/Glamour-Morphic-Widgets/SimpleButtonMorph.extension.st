Extension { #name : #SimpleButtonMorph }

{ #category : #'*glamour-morphic-widgets' }
SimpleButtonMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]
