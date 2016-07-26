accessing
previousInspectorType
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index - 1.