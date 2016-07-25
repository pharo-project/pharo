initialize-release
resetState 
	"Establish the initial conditions for editing the paragraph: place caret 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex := 1.	"Like pointBlock in EnhancedTextEditor"
	markIndex := 1.	"Like markIndex in EnhancedTextEditor"
	beginTypeInIndex := nil.	"Like beginTypeInBlock in EnhancedTextEditor"
	selectionShowing := false