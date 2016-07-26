layout
fullBounds
	fullBounds ifNotNil: [^ fullBounds].
	^ submorphs 
		ifEmpty: [bounds]
		ifNotEmpty: [
			self doLayoutIn: self layoutBounds.
			fullBounds]