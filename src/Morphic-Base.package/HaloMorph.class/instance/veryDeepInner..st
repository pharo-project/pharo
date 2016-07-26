copying
veryDeepInner: deepCopier
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

	super veryDeepInner: deepCopier.
	"target := target.		Weakly copied"
	"innerTarget := innerTarget.		Weakly copied"
	positionOffset := positionOffset veryDeepCopyWith: deepCopier.
	angleOffset := angleOffset veryDeepCopyWith: deepCopier.
	growingOrRotating := growingOrRotating veryDeepCopyWith: deepCopier.
	directionArrowAnchor := directionArrowAnchor.
	haloBox := haloBox.
	originalExtent := originalExtent.
	nameMorph := nameMorph.
