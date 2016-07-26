private
setDefaults
	self hasScheme 
		ifFalse: [ self scheme: self schemeOrDefault ].
	self hasPort 
		ifFalse: [ self port: self portOrDefault ]    