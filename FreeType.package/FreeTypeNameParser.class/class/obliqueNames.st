known names
obliqueNames
	"Answer a sequence of String tokens that indicate an oblique font
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #obliqueNames put: nil.
	"
	obliqueNames ifNotNil:[^obliqueNames].
	^obliqueNames := #(
		'inclined'
		'oblique'
		'backslanted'
		'backslant'
		'slanted').