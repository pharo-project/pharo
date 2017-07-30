private validating
validateClassName
	"Validate the new class name. Raise warning if invalid."
	name isValidGlobalName ifFalse: [ 
		InvalidGlobalName signal: 'Class name is not a valid global name. It must start with uppercase letter and continue with alphanumeric characters or underscore. Default names used in class or trait templates are not allowed.' for: name  ].