*GT-InspectorExtensions-Core
gtInspectorVariableNodesIn: aCollection

	aCollection addAll: (self literals collectWithIndex: [ :aLiteral :anIndex | 
		GTInspectorDynamicNode hostObject: self label: ('literal', anIndex asString) value: aLiteral ]).
	self initialPC to: self size do: [ :index |
		aCollection add: (GTInspectorDynamicNode hostObject: self label: ('bc ', index asString) value: (self at: index) ) ] 
	
