*GT-InspectorExtensions-Core
gtInspectorVariableNodesIn: aCollection
	
	aCollection
		add: (GTInspectorDynamicNode hostObject: self label: '1' value: (self at: 1));
		add: (GTInspectorDynamicNode hostObject: self label: '2' value: (self at: 2))