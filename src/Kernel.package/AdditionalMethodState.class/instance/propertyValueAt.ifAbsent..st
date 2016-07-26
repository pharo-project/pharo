properties
propertyValueAt: aKey ifAbsent: aBlock
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."

	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^propertyOrPragma value]].
	^aBlock value