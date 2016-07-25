copying
veryDeepFixupWith: deepCopier
	"If target and arguments fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

super veryDeepFixupWith: deepCopier.
target := deepCopier references at: target ifAbsent: [target].
arguments := arguments collect: [:each |
	deepCopier references at: each ifAbsent: [each]].
