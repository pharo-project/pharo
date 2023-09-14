Extension { #name : #FormCanvas }

{ #category : #'*Rubric-Editing-Core' }
FormCanvas >> rubParagraph: para bounds: bounds color: c [

	^ self rubParagraph: para bounds: bounds color: c scale: 1
]

{ #category : #'*Rubric-Editing-Core' }
FormCanvas >> rubParagraph: para bounds: bounds color: c scale: scale [

	| scanner |
	self setPaintColor: c.
	scanner := (port clippedBy: (bounds translateBy: origin)) rubDisplayScannerFor: para
		foreground: c.
	scanner scale: scale.
	para drawOn: (self copyClipRect: bounds) using: scanner at: origin + bounds topLeft
]
