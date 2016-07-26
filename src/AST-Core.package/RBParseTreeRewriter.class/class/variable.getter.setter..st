instance creation
variable: aVarName getter: getMethod setter: setMethod 
	^self 
		variable: aVarName
		getter: getMethod
		setter: setMethod
		receiver: 'self'