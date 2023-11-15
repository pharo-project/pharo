Extension { #name : 'MorphicUIManager' }

{ #category : '*MonticelloGUI-Diff' }
MorphicUIManager >> merge: merger informing: aString [

	| mergeMorph  window |
	mergeMorph := PSMCMergeMorph forMerger: merger.
	mergeMorph
		fromDescription: 'Working copy' translated;
		toDescription: aString.
	window := mergeMorph newWindow
		title: aString;
		yourself.
	window openModal.
	^ mergeMorph merged
]
