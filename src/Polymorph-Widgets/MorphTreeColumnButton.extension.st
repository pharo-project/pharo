Extension { #name : #MorphTreeColumnButton }

{ #category : #'*Polymorph-Widgets' }
MorphTreeColumnButton >> themeChanged [
	self fillStyle: (self theme morphTreeColumnFillStyleFor: self).
	self borderStyle: (self theme morphTreeColumnBorderStyleFor: self).
	super themeChanged
]
