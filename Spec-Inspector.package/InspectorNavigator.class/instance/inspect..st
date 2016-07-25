actions
inspect: anObject
	| oldInspectorType |
	oldInspectorType := self inspectorType.
	self 
		inspector: (EyeInspector inspector: anObject);
		inspectorType: oldInspectorType.
