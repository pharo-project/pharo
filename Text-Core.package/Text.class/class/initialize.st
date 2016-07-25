initialization
initialize	"Text initialize"
	"Initialize constants shared by classes associated with text display."

	TextSharedInformation at: #CaretForm put:
				(Form extent: 16@5
					fromArray: #(2r001100e26 2r001100e26 2r011110e26 2r111111e26 2r110011e26)
					offset: -3@0).