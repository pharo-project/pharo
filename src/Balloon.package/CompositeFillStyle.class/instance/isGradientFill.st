testing
isGradientFill
	"Answer whether any of the composited fill styles are gradients."

	self fillStyles reverseDo: [:fs |
		fs isGradientFill ifTrue: [^true]].
	^false