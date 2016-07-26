properties
propertyKeysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[aBlock value: propertyOrPragma key value: propertyOrPragma value]]