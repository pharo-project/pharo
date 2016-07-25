accessing
at: aKey ifAbsentPut: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	1 to: self basicSize do:
		[:i |
		| propertyOrPragma "<Association|Pragma>" |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^method propertyValueAt: aKey put: aBlock value