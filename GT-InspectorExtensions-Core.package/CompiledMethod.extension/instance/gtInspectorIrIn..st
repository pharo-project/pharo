*GT-InspectorExtensions-Core
gtInspectorIrIn: composite
	<gtInspectorPresentationOrder: 35> 
	composite text 
		title: 'Ir';
		display: [ :anObject || string |
				string := anObject ir longPrintString.
				string copyFrom: 2 to: string size ]