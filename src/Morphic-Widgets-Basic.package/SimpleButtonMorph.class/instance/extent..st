geometry
extent: newExtent

	super extent: newExtent.
	label ifNotNil: [label position: self center - (label extent // 2)].