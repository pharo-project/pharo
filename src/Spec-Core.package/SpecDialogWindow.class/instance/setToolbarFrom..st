protocol
setToolbarFrom: aBlock
	| newToolbar |
	
	newToolbar := aBlock value.
	
	toolbar := newToolbar.
	newToolbar owner: self