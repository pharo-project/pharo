Extension { #name : 'MCMergeResolutionRequest' }

{ #category : '*Monticello-GUI-Diff' }
MCMergeResolutionRequest >> defaultAction [
	"Modally open a merge tool."

	^self viewMerger
]

{ #category : '*Monticello-GUI-Diff' }
MCMergeResolutionRequest >> viewMerger [
	"Open a model browser to perform the merge and answer wheter merged."

	^PSMCPatchMorph usedByDefault
		ifTrue: [self viewPatchMerger]
		ifFalse: [(MCMergeBrowser new
					merger: merger;
					label: messageText) showModally]
]

{ #category : '*Monticello-GUI-Diff' }
MCMergeResolutionRequest >> viewPatchMerger [
	"Open a modal diff tools browser to perform the merge."

	^ UIManager default merge: self merger informing: messageText
]
