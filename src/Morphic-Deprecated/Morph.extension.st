Extension { #name : 'Morph' }

{ #category : '*Morphic-Deprecated' }
Morph >> addFlexShell [
	"Wrap a rotating and scaling shell around this morph."

	self
		deprecated: 'Use TransformationMorph flexing: instead'
		transformWith: '`@receiver addFlexShell' -> 'TransformationMorph flexing: `@receiver'.
	^ TransformationMorph flexing: self
]
