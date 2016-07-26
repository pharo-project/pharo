handles
addGrabHandle: haloSpec
	"If appropriate, add the black halo handle for picking up the target"

	self addHandle: haloSpec on: #mouseDown send: #doGrab:with: to: self

