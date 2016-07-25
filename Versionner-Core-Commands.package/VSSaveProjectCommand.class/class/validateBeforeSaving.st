settings
validateBeforeSaving

	^ ValidateBeforeSaving ifNil: [ ValidateBeforeSaving := true ]