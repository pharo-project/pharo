*GT-InspectorExtensions-Core
gtInspectorKeysIn: composite
	<gtInspectorPresentationOrder: 1>

	^ composite fastList
		title: 'Keys';
		useCache;
		display: [ self keys ]