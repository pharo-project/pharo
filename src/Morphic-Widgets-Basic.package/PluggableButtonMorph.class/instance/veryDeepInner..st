copying
veryDeepInner: deepCopier
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"model := model.		Weakly copied"
label := label veryDeepCopyWith: deepCopier.
icon :=  icon veryDeepCopyWith: deepCopier.
"getStateSelector := getStateSelector.		a Symbol"
"actionSelector := actionSelector.		a Symbol"
"getLabelSelector := getLabelSelector.		a Symbol"
"getMenuSelector := getMenuSelector.		a Symbol"
shortcutCharacter := shortcutCharacter veryDeepCopyWith: deepCopier.
askBeforeChanging := askBeforeChanging veryDeepCopyWith: deepCopier.
triggerOnMouseDown := triggerOnMouseDown veryDeepCopyWith: deepCopier.
offColor := offColor veryDeepCopyWith: deepCopier.
onColor := onColor veryDeepCopyWith: deepCopier.
feedbackColor := feedbackColor veryDeepCopyWith: deepCopier.
showSelectionFeedback := showSelectionFeedback veryDeepCopyWith: deepCopier.
allButtons := nil.		"a cache"
arguments := arguments veryDeepCopyWith: deepCopier.
argumentsProvider := argumentsProvider veryDeepCopyWith: deepCopier.
argumentsSelector := argumentsSelector.  " a Symbol" 