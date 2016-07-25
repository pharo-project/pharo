finalize resources
finalizeResourceData: data
	| handle id contextHandle |
	handle := data first.
	id := data second.
	contextHandle := data third.
	
	(self findSurface: id) = handle value ifFalse: [ 
		"self error: 'surface is not registered with surface plugin'."
		^ self ].
	
	self unregisterSurfaceWithId: id.
	self destroyContextHandle: contextHandle value. 
	self destroySurfaceHandle: handle value
