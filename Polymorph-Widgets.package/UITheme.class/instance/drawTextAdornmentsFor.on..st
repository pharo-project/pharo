border-styles
drawTextAdornmentsFor: aPluggableTextMorph on: aCanvas
	"Indicate edit status for the given morph."

	(aPluggableTextMorph hasEditingConflicts or: [ aPluggableTextMorph hasValidText not ])
		ifTrue: [ ^ self drawTextAdornmentFor: aPluggableTextMorph color: Color red on: aCanvas ].
	aPluggableTextMorph hasUnacceptedEdits
		ifTrue: [ aPluggableTextMorph model wantsVisualFeedback
				ifTrue: [ ^ self drawTextAdornmentFor: aPluggableTextMorph color: Color orange on: aCanvas ] ]