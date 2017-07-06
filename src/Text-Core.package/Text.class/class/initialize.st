initialization
initialize	"Text initialize"
	"Initialize constants shared by classes associated with text display."

	TextSharedInformation at: #CaretForm put:
				(Form extent: 16@5
					fromArray: #(	
						2r00110000000000000000000000000000
						2r00110000000000000000000000000000
					 	2r01111000000000000000000000000000
					 	2r11111100000000000000000000000000
					 	2r11001100000000000000000000000000)
					offset: -3@0).