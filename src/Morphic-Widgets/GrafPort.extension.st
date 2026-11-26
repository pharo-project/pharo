Extension { #name : 'GrafPort' }

{ #category : '*Morphic-Widgets' }
GrafPort >> displayScannerFor: para foreground: foreColor [
	^ (BitBltDisplayScanner new text: para text textStyle: para textStyle
			foreground: foreColor)
		setPort: self shallowCopy
]

{ #category : '*Morphic-Widgets' }
GrafPort >> rubDisplayScannerFor: para foreground: foreColor [
	^ (RubDisplayScanner new text: para text textStyle: para textStyle
			foreground: foreColor)
		setPort: self shallowCopy
]
