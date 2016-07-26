testing
isOrientedFill
	"Answer whether any of the composited fill styles are oriented."

	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [^true]].
	^false