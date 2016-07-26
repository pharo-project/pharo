changes
viewChanges: patch from: fromDescription to: toDescription title: title
	"Open a browser on the given patch."

	^ ( PSMCPatchMorph forPatch:  patch)
			fromDescription: fromDescription;
			toDescription:  toDescription;
			newWindow;
			title: title;
			open 