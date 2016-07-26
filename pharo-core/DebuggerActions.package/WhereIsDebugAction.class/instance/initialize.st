initialization
initialize

	super initialize.
	"This action just updates the selection in the code editor. 
	No need to do any update or validation."
	self needsUpdate: false.
	self needsValidation: false.
	self withSeparatorAfter