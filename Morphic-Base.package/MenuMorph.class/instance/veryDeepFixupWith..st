copying
veryDeepFixupWith: deepCopier
	"If fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals."

super veryDeepFixupWith: deepCopier.
defaultTarget := deepCopier references at: defaultTarget ifAbsent: [defaultTarget].
popUpOwner := deepCopier references at: popUpOwner ifAbsent: [popUpOwner].
activeSubMenu := deepCopier references at: activeSubMenu ifAbsent:[activeSubMenu].