Extension { #name : 'GrafPort' }

{ #category : '*Morphic-Base' }
GrafPort >> displayScannerFor: para foreground: foreColor [
	^ (BitBltDisplayScanner new text: para text textStyle: para textStyle
			foreground: foreColor)
		setPort: self shallowCopy
]
