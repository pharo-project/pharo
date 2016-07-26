opening
open: aRenderable
	window := GLMMorphicWindowRenderer render: aRenderable from: self.
	^ window openInWorld 