rendering
render: aPane
	"we just register for the announcement"

	aPane when: GLMMatchingPresentationsChanged send: #actOnMatchingPresentationsChanged: to: self