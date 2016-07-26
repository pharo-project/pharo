accessing
properties

	| propertyStream |
	propertyStream := WriteStream on: (Array new: self basicSize * 2).
	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[propertyStream nextPut: propertyOrPragma key; nextPut: propertyOrPragma value]].
	^IdentityDictionary newFromPairs: propertyStream contents