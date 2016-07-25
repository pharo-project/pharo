accessing
target: aMorph

	target := aMorph.
	aMorph ifNotNil: [
		self fillStyle: (aMorph theme resizerGripNormalFillStyleFor: self)]