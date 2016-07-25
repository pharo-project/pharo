caching
fullReleaseCachedState
	"Release the cached state of the receiver and its full submorph tree."

	self allMorphsDo: [:m | m releaseCachedState].
