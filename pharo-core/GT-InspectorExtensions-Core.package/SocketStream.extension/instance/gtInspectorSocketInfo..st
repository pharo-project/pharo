*GT-InspectorExtensions-Core
gtInspectorSocketInfo: composite
	<gtInspectorPresentationOrder: 5>
	^ self socket ifNotNil: [ :aSocket | aSocket gtInspectorSocketInfo: composite ]