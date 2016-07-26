surface management
createSurface
	| extent |
	extent := bounds extent asIntegerPoint.
	surface := AthensCairoSurface extent: extent.
