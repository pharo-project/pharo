accessing
normal
	"Answer an effective normal of any oriented fill styles.
	Answer the top-left minima (probably not an accurate assumption)."

	|normal|
	normal := nil.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			normal := normal
				ifNil: [fs normal]
				ifNotNil: [normal min: fs normal]]].
	^normal ifNil: [0@0] "just in case"