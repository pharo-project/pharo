accessing
lastPage
	| p |
	p := (self nodeList size / pageSize) asInteger.
	self nodeList size \\ pageSize > 0 ifTrue: [p := p + 1].
	^ p.
