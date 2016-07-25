copying
copy
	"we are overriding the copy method simply to retain the tree of all prototype presentations. 
	this is useful for debugging."

	| copy |
	self flag: 'perhaps this should be replaced with smarter analysys mechanisms'.
	copy := super copy.
	copy parentPrototype: self.
	^ copy