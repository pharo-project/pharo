copying
veryDeepInner: deepCopier
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"target := target.		Weakly copied"
"actionSelector := actionSelector.		a Symbol"
"arguments := arguments.		All weakly copied"
actWhen := actWhen veryDeepCopyWith: deepCopier.
oldColor := oldColor veryDeepCopyWith: deepCopier.
label := label veryDeepCopyWith: deepCopier.