accessing
setMethod: aMethod
	method := aMethod.
	1 to: self basicSize do:
		[:i| | propertyOrPragma "<Association|Pragma>" |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[propertyOrPragma setMethod: aMethod]]