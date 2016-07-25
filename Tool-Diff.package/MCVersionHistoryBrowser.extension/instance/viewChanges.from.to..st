*Tool-Diff
viewChanges: patch from: fromDescription to: toDescription
	"Open a browser on the patch."
	
	|patchLabel|
	patchLabel := 'Changes between {1} and {2}' translated format: {fromDescription. toDescription}.
	PSMCPatchMorph usedByDefault
		ifTrue: [((PSMCPatchMorph forPatch: patch)
						fromDescription: fromDescription;
						toDescription: toDescription;
						newWindow)
					title: patchLabel;
					open]
		ifFalse: [(MCPatchBrowser forPatch: patch)
					label: patchLabel;
					show]