copying
veryDeepCopyWith: deepCopier
	"Copy me and the entire tree of objects I point to.  An object in the tree twice is copied once, and both references point to him.  deepCopier holds a dictionary of objects we have seen.  See veryDeepInner:, veryDeepFixupWith:"

	self prepareToBeSaved.
	^ super veryDeepCopyWith: deepCopier