*Tool-Diff
viewMerger
	"Open a model browser to perform the merge and answer wheter merged."

	^PSMCPatchMorph usedByDefault
		ifTrue: [self viewPatchMerger]
		ifFalse: [(MCMergeBrowser new
					merger: merger;
					label: messageText) showModally]