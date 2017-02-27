An ExternalForm is a specialized Form whose pixel-data is stored in memory that the user provides a pointer to.  This can simply be memory on the C heap, or (the motivating use-case...) it can be a pointer that is temporarily "mapped" from GPU memory by an API such as OpenCL.

IMPORTANT: Moved form FFI to SDL2 because is not needed there anymore but OSWindow-SDL2 package uses it.

The user is responsible for both releasing the image-memory, as well as destroying the surface handle (perhaps the latter should be handled by automatic finalization).

Example usage:

| extent form ptr |
extent := 400@300.
form := ExternalForm extent: extent depth: 32.
ptr := ExternalAddress gcallocate: (extent x * extent y * 4).
form setManualSurfacePointer: ptr.
Display displayScaledOn: form.
form displayAt: 0@0.
form destroySurface.
