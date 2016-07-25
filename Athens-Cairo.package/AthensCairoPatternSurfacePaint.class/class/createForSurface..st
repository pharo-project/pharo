as yet unclassified
createForSurface: anAthensCairoSurface
	|pat|
	pat := self primCreateForSurface: anAthensCairoSurface .
	pat initializeFor: anAthensCairoSurface  .
	^ pat