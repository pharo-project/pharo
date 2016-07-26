examples
simpleErrorPopper
	"
	self simpleErrorPopper
	"

	(GLMErrorPopper new 
		withString: 'error message goes here' 
		from: RubScrolledTextMorph new) openInWorld