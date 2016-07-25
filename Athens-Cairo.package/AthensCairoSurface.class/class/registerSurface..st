private
registerSurface: anAthensCairoSurface
	"register the cairo surface with surface plugin,
	so, it can be used directly by bitblt operations.
	Answer an id and unique session object"
	| id address |

	self checkSession.

	id := ByteArray new: 4.
	address := anAthensCairoSurface handle value.
	(self ioRegisterSurface: address dispatch: dispatch surfaceId: id)
		ifFalse: [ self error: 'Unable to register surface with SurfacePlugin' ].
	id := id signedLongAt: 1.
	
	^ id