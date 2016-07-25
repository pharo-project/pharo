accessing
removeKey: aKey ifAbsent: aBlock
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."
	
	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma key]
				ifFalse: [propertyOrPragma keyword])
			== aKey ifTrue:
			[^method removeProperty: aKey]].
	^aBlock value