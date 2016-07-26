ui requests
merge: merger informing: aString

	| mergeMorph  window |
	mergeMorph := PSMCMergeMorph forMerger: merger.
	mergeMorph
		fromDescription: 'Working copy' translated;
		toDescription: aString.
	window := mergeMorph newWindow
		title: aString;
		yourself.
	self modalMorph openModal: window.
	^ mergeMorph merged