Extension { #name : #DateAndTime }

{ #category : #'*GT-InspectorExtensions-Core' }
DateAndTime >> gtInspectorDetailsIn: composite [
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
					'hours' -> self hours.
					'minutes' -> self minutes.
					'seconds' -> self seconds.
					'nanoseconds' -> self nanoSecond.
					'meridian' -> self meridianAbbreviation.
					'offset' -> self offset.
					'date' -> self asDate.
					'time' -> self asTime.
					'iso' -> self printString.
					'utc' -> self asUTC } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]
