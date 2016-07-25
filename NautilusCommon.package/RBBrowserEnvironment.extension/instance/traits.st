*NautilusCommon
traits
	| traits |
	traits := IdentitySet new: 4096.
	self traitsDo: [ :each | traits add: each ].
	^ traits