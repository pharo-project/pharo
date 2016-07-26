handles
addDupHandle: haloSpec
	"Add the halo that offers duplication, or, when shift is down, make-sibling"

	self addHandle: haloSpec on: #mouseDown send: #doDup:with: to: self

