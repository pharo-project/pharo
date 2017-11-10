Extension { #name : #Duration }

{ #category : #'*GT-InspectorExtensions-Core' }
Duration >> gtInspectorDetailsIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: 'Details';
		display: [ 
					{'human readable' -> self humanReadablePrintString.
					'days' -> self days.
					'hours' -> self hours.
					'minutes' -> self minutes.
					'seconds' -> self seconds .
					'nanoseconds' -> self nanoSeconds } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]
