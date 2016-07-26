caching
releaseCachedState
	| oo |
	super releaseCachedState.
	cacheCanvas := nil.
	oo := owner.
	self removeAllMorphs.
	self initialize.	"nuke everything"
	self privateOwner: oo.
	self releaseAllFocus.