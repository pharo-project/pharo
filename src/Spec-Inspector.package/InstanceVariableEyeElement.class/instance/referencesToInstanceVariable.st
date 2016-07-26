action
referencesToInstanceVariable
	"Open a browser on all references to the selected instance variable, if that's what currently selected."

	^ self systemNavigation browseAllAccessesTo: self instVarName from: self hostClass