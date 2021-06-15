Extension { #name : #GIFReadWriter }

{ #category : #'*Deprecated90' }
GIFReadWriter >> delay [
	^ self
		deprecated: 'Delay values for animaed GIFs are no longer set by the GIFReadWriter. Set them in each AnimatedImageFrame instead! See GIFReaderWriter class >> #exampleAnim for an example.'
]

{ #category : #'*Deprecated90' }
GIFReadWriter >> delay: anInteger [
	^ self
		deprecated: 'Delay values for animaed GIFs are no longer set by the GIFReadWriter. Set them in each AnimatedImageFrame instead! See GIFReaderWriter class >> #exampleAnim for an example.'
]
