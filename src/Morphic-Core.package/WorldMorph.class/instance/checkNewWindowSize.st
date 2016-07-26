updating screen
checkNewWindowSize
	| newExtent |
	newExtent := self osWindow extent.
	newExtent ~= self extent ifTrue: [
		self osWindowRenderer newExtent: newExtent.
		self extent: newExtent.
	]