operations
timeString
	| aDateTime |
	aDateTime := self log timeAt: entry ifAbsent: [ ^ '' ].
	
	^ String streamContents: [ :stream |
		"FIX: at least put it in another place"
		aDateTime asDate = Date today 
			ifFalse: [ 
				stream 
					nextPutAll: aDateTime asDate ddmmyyyy;
					space ].

		aDateTime asTime 
			print24: true 
			showSeconds: false 
			on: stream.
		]