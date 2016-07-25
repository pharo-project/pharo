copying
veryDeepInner: deepCopier
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

	super veryDeepInner: deepCopier.
	"defaultTarget := defaultTarget.		Weakly copied"
	selectedItem := selectedItem veryDeepCopyWith: deepCopier.
	stayUp := stayUp veryDeepCopyWith: deepCopier.
	popUpOwner := popUpOwner.		"Weakly copied"
	activeSubMenu := activeSubMenu. "Weakly copied"
