commands
width: aNumber
	"Doesn't work when opened in window, only when part of another layout"
	self send: #width: withArguments: {aNumber}