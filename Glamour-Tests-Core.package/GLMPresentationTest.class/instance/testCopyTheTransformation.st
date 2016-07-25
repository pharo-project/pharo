tests
testCopyTheTransformation
	| presentation newPresentation transformation |
	presentation := GLMPresentation new.
	presentation selectionTransformation: [:each | each + 1].
	newPresentation := presentation copy.
	self assert: (presentation selectionTransformation value: 1) equals: 2.
	self assert: (newPresentation selectionTransformation value: 1) equals: 2.