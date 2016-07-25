reordering
moveDown: aLink
	|  e1 e2 e3 e4  |
	
	(e3 := aLink nextLink) ifNil:[^self].
	e2 := aLink.
	e4 := e3 nextLink.
	e1 := e2 previousLink.
	"swap e2 & e3"
	e1 ifNotNil:[e1 nextLink: e2].
	e2 nextLink: e3.
	e3 nextLink: e4.
	e4 ifNotNil:[e4 previousLink: e3].
	e3 previousLink: e2.
	e2 previousLink: e1

	