geometry
textFieldHeight

	^ customHeight
		ifNil: [ self textArea height ]
		ifNotNil: [ customHeight brickValue: self ]