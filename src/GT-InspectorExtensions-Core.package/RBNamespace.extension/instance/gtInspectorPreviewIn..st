*GT-InspectorExtensions-Core
gtInspectorPreviewIn: composite
	<gtInspectorPresentationOrder: 0>
	self changes isNil ifTrue: [ ^ self ].
	self changes gtInspectorPreviewIn: composite