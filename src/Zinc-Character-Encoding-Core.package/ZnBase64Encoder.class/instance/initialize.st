initialization
initialize
	super initialize.
	alphabet := DefaultAlphabet.
	inverse := DefaultInverse.
	self whitespace: #any