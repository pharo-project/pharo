composition
normalizeTransformations
	self transformations: (
		self transformations collect: [:each |
			each normalized])