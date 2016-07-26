private-extensions-date
materializeFromForDate: aDecoder
	<flExtensionOf: #Date selector: #materializeFrom:>
	<flExtensionClassSide>
	^ self 
		starting: (DateAndTime materializeFrom: aDecoder)
		duration: (Duration days: 1)