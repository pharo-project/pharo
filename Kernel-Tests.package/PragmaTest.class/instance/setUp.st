running
setUp

	atPragma := Pragma for: (Object methodDict at: #at:) selector: #primitive: arguments: (Array with: 60).
	anotherAtPragma := Pragma for: (Object methodDict at: #at:) selector: #primitive: arguments: (Array with: 60).
	yetAnotherAtPragma := Pragma for: (Object methodDict at: #at:) selector: #primitive: arguments: (Array with: 60).
	atPutPragma := Pragma for: (Object methodDict at: #at:) selector: #primitive: arguments: (Array with: 61).
