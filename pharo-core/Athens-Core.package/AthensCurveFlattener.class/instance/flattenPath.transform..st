public API
flattenPath: aPath transform: aTransformation

	"This is an entry point for flattening path.
	An additional argument, transform is an Affine matrix, used to map path geometry to screen,
	and therefore used to calculate the error tolerance for path subdivisions"
	
	^ self flattenPath: aPath transform: aTransformation toleranceMultiplier: 1 