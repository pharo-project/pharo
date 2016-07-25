*GT-InspectorExtensions-Core
gtInspectorMappingsIn: composite
	<gtInspectorPresentationOrder: 95>
	(self hasProperty: #kmDispatcher) ifFalse: [^self].
	self kmDispatcher gtInspectorMappingsIn: composite