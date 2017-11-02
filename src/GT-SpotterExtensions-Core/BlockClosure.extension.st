Extension { #name : #BlockClosure }

{ #category : #'*GT-SpotterExtensions-Core' }
BlockClosure >> gtSpotterCodePreviewIn: aComposite [
	<spotterPreview: 10>

	^ self sourceNode gtSpotterCodePreviewIn: aComposite
]
