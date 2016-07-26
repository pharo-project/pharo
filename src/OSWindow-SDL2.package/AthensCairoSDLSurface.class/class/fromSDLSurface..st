instance creation
fromSDLSurface: surface 
	^ ( self 
		primImageFromData: surface pixels 
		width: surface w 
		height: surface h
		pitch: surface pitch) initialize