instance creation
cleanUpSourceInTrailers

	self allInstances do: [:e | e isInstalled ifFalse: [e embeddSourceInTrailer]].  
	"pay attention since embeddSourceInTrailer creates a new compiled method. So iterating while
	changing it is a bad idea. This is why we use allInstances do and not allInstancesDo:"