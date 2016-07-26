*GT-InspectorExtensions-Core
gtInspectorPreviewIn: composite
	<gtInspectorPresentationOrder: 30>
	
	self widget ifNotNil: [ :w | w gtInspectorPreviewIn: composite ]