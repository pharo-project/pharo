*OpalTools
gtInspectorIrIn: composite
	<gtInspectorPresentationOrder: 35> 
	composite text 
		title: 'Symbolic';
		display: [ :anObject || string |
				string := anObject longPrintString.
				string copyFrom: 2 to: string size ]