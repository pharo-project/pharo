navigation
computedHeightFromContents
	| h |
	h := 0.
	self submorphsDo: [:sm | h := h max: sm height].
	^ h
