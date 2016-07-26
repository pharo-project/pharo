known names
italicNames
	"Answer a sequence of String tokens that indicate an italic font
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #italicNames put: nil.
	"
	italicNames ifNotNil:[^italicNames].
	^italicNames := #(
		'ita'
		'ital'
		'italic'
		'cursive'
		'kursiv').