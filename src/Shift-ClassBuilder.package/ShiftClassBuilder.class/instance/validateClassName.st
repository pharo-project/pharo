private validating
validateClassName
	name ifNil: [ ^self ].
	
	"I try to convert to symbol, if there is an error the next guard will catch it"
	[name := name asSymbol ] on:Error do: [  ].
	
	name isSymbol ifFalse:[InvalidGlobalName 
				signal: 'Global names should be symbols' 
				for: name].

	name isValidGlobalName ifFalse: [ 
		InvalidGlobalName signal: 'Class name is not a valid global name. It must start with uppercase letter and continue with alphanumeric characters or underscore. Default names used in class or trait templates are not allowed.' for: name  ].
	
		
	DangerousClassNotifier check: name 