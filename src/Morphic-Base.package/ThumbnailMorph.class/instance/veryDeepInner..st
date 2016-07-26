copying
veryDeepInner: deepCopier
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"objectToView := objectToView.		Weakly copied"
viewSelector := viewSelector veryDeepCopyWith: deepCopier.
lastSketchForm := lastSketchForm veryDeepCopyWith: deepCopier.
lastFormShown := lastFormShown veryDeepCopyWith: deepCopier.
drawTime := drawTime veryDeepCopyWith: deepCopier.
