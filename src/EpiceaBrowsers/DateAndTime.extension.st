Extension { #name : #DateAndTime }

{ #category : #'*EpiceaBrowsers' }
DateAndTime >> epiceaBrowsersAsString [

	^ String streamContents: [ :stream |
		stream 
			nextPutAll: self asDate epiceaBrowsersAsString;
			nextPutAll: ', '.
		self asTime 
			print24: false
			showSeconds: false 
			on: stream ]
]
