initialization
initialize
	super initialize.
	self rewriteRule
		replace: '``@object withFirstCharacterDownshifted' with: '``@object uncapitalized'.

