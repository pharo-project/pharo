accessing
layout: aSymbol
	"Set the value of layout"

	|old|
	(old := layout) = aSymbol ifTrue: [^self].
	layout := aSymbol.
	((old = #scaled or: [old = #scaledAspect]) or: [aSymbol = #scaled or: [aSymbol = #scaledAspect]])
		ifTrue: [self cachedForm: nil].
	self changed