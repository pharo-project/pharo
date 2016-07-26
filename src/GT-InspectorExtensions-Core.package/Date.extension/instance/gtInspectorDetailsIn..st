*GT-InspectorExtensions-Core
gtInspectorDetailsIn: composite
	<gtInspectorPresentationOrder: 30>
	^ composite table
		title: [ 'Details' ];
		display: [ 
					{'year' -> self year.
					'month' -> self monthIndex.
					'month name' -> self monthName.
					'day of month' -> self dayOfMonth.
					'day of week' -> self dayOfWeekName.
					'day of year' -> self dayOfYear.
					'iso' -> self yyyymmdd } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value