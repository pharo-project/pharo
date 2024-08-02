Extension { #name : 'MCMergeResolutionRequest' }

{ #category : '*MonticelloGUI-Diff' }
MCMergeResolutionRequest >> defaultAction [
	"Modally open a merge tool."

	| mergeMorph window |
	mergeMorph := PSMCMergeMorph forMerger: merger.
	mergeMorph
		fromDescription: 'Working copy' translated;
		toDescription: messageText.
	window := mergeMorph newWindow
		          title: messageText;
		          yourself.
	window openModal.
	^ mergeMorph merged
]
