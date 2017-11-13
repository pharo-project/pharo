Extension { #name : #Date }

{ #category : #'*GT-InspectorExtensions-Core' }
Date >> gtInspectorDetailsIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: 'Details';
		display: [ 
					{'iso' -> self yyyymmdd.
					'year' -> self year.
					'month' -> self monthIndex.
					'month name' -> self monthName.
					'day of month' -> self dayOfMonth.
					'day of week' -> self dayOfWeekName.
					'day of year' -> self dayOfYear } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]

{ #category : #'*GT-InspectorExtensions-Core' }
Date >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite morph
		title: 'Calendar';
		morph: [ CalendarMorph on: self ]
]
