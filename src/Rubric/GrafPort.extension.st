Extension { #name : #GrafPort }

{ #category : #'*Rubric-Editing-Core' }
GrafPort >> rubDisplayScannerFor: para foreground: foreColor [
	^ (RubDisplayScanner new text: para text textStyle: para textStyle
			foreground: foreColor)
		setPort: self shallowCopy
]
