Extension { #name : #TextMorph }

{ #category : #'*glamour-morphic-widgets' }
TextMorph >> asReadOnlyMorph [
	^ StringMorph contents: self text
]

{ #category : #'*glamour-morphic-widgets' }
TextMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]
