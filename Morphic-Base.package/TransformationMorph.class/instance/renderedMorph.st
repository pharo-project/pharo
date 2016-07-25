classification
renderedMorph
"We are a renderer. Answer appropriately."

	submorphs isEmpty ifTrue: [^self].
	^self firstSubmorph renderedMorph