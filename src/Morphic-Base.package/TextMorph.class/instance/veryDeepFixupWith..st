copying
veryDeepFixupWith: deepCopier 
	"If target and arguments fields were weakly copied, fix them here.  If 
	they were in the tree being copied, fix them up, otherwise point to the 
	originals!"

	super veryDeepFixupWith: deepCopier.
	"It makes no sense to share pointers to an existing predecessor and successor"
	predecessor := deepCopier references at: predecessor ifAbsent: [nil].
	successor := deepCopier references at: successor ifAbsent: [nil]