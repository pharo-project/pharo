accessing validation
shouldValidate
	^ shouldValidate ifNil: [shouldValidate := false]