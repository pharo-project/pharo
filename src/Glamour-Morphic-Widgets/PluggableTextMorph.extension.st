Extension { #name : #PluggableTextMorph }

{ #category : #'*glamour-morphic-widgets' }
PluggableTextMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]
