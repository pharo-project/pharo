private
createSDLSurfaceWithExtent: extent
	surfaceForm ifNotNil: [ surfaceForm destroySurface ].
	"createRGBSurfaceForCairoWidth creates the surface with autoRelease."
	sdlSurface := SDL2 createRGBSurfaceForCairoWidth: extent x height: extent y.
	surfaceForm := ExternalForm extent: extent depth: 32 bits: sdlSurface pixels.