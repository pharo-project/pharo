private
dateTimeStringFor: dateTime

	^ String streamContents: [ :stream |
		stream 
			nextPutAll: dateTime asDate ddmmyyyy;
			space.
		dateTime asTime 
			print24: true 
			showSeconds: false 
			on: stream ]