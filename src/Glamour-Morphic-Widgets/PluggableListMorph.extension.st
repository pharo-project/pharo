Extension { #name : #PluggableListMorph }

{ #category : #'*glamour-morphic-widgets' }
PluggableListMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]
