enumerating
reverseDo: block
	"Execute block for each of my elements, in reverse order."

	self reverseLinksDo: [ :each | block value: each value ]