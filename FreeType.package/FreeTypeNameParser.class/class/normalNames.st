known names
normalNames
	"Answer a sequence of String tokens that indicate a Regular
	(i.e. non-oblique, non-italic) font within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #normalNames put: nil.
	"
	normalNames ifNotNil:[^normalNames].
	^normalNames := #('Book' 'Normal' 'Regular' 'Roman' 'Upright').