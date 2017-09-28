accessing
layout: aLayout
	layout := aLayout.
	layout host: target.
	layout finalize.
	layout = target classLayout
		ifFalse: [ self computeChange ]