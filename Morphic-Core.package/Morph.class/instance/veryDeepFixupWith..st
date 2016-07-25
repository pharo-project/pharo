copying
veryDeepFixupWith: deepCopier
	"If some fields were weakly copied, fix new copy here."

	"super veryDeepFixupWith: deepCopier.	Object has no fixups, so don't call it"

	"If my owner is being duplicated too, then store his duplicate.
	 If I am owned outside the duplicated tree, then I am no longer owned!"
	owner := deepCopier references at: owner ifAbsent: [nil].

