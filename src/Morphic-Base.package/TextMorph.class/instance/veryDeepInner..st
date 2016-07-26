copying
veryDeepInner: deepCopier 
	"Copy all of my instance variables. Some need to be not copied at all, but shared.
	Warning!! Every instance variable defined in this class must be handled.
	We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

	super veryDeepInner: deepCopier.
	textStyle := textStyle veryDeepCopyWith: deepCopier.
	text := text veryDeepCopyWith: deepCopier.
	wrapFlag := wrapFlag veryDeepCopyWith: deepCopier.
	paragraph := paragraph veryDeepCopyWith: deepCopier.
	editor := editor veryDeepCopyWith: deepCopier.
	container := container veryDeepCopyWith: deepCopier.
	predecessor := predecessor.
	successor := successor.
	backgroundColor := backgroundColor veryDeepCopyWith: deepCopier.
	margins := margins veryDeepCopyWith: deepCopier.
