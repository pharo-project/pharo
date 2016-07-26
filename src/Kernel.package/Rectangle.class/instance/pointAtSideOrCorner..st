accessing
pointAtSideOrCorner: loc
	"Answer the point represented by the given location."
	
	^ self
		perform: (#(topLeft topCenter topRight rightCenter
					bottomRight bottomCenter bottomLeft leftCenter)
						at: (#(topLeft top topRight right
					bottomRight bottom bottomLeft left) indexOf: loc))

	