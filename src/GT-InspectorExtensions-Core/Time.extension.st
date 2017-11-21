Extension { #name : #Time }

{ #category : #'*GT-InspectorExtensions-Core' }
Time >> gtInspectorDetailsIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: 'Details';
		display: [ 
					{'iso' -> self print24.
					'hours' -> self hours.
					'minutes' -> self minutes.
					'seconds' -> self seconds.
					'nanoseconds' -> self nanoSecond.
					'meridian' -> self meridianAbbreviation } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]
