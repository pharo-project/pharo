private
colorFor: index
	index odd ifTrue: [ ^ Smalltalk ui theme lightBackgroundColor ].
	^ Smalltalk ui theme backgroundColor