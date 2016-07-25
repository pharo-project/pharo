display
viewChanges: patch from: fromDescription to: toDescription in: workingCopy
	"Open a browser on the given patch."

	PSMCPatchMorph usedByDefault
		ifTrue: [((PSMCPatchMorph forPatch: patch)
						fromDescription: fromDescription;
						toDescription: toDescription;
						newWindow)
					title: ('Changes to {1}' translated format: {fromDescription});
					open]
		ifFalse: [(MCPatchBrowser forPatch: patch)
					label: 'Patch Browser: ', workingCopy description;
					show]