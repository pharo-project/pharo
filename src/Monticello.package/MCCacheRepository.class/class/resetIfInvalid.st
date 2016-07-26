utility
resetIfInvalid

	default notNil and: [default directory exists ifFalse: [default := nil]]