Extension { #name : #DateAndTime }

{ #category : #'*Fuel-Core' }
DateAndTime >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitHookPrimitive: self

]

{ #category : #'*Fuel-Core' }
DateAndTime >> fuelSet: julianDay nanoSecond: nanoSeconds seconds: numberOfSeconds offset: anOffset [
	self setJdn: julianDay seconds: numberOfSeconds nano: nanoSeconds offset: anOffset
]

{ #category : #'*Fuel-Core' }
DateAndTime class >> materializeFrom: aDecoder [

	^ self basicNew
		fuelSet: aDecoder nextEncodedUint32
		nanoSecond: aDecoder nextEncodedUint32
		seconds: aDecoder nextEncodedInt24
		offset: (Duration 
			seconds: aDecoder nextEncodedInt24 
			nanoSeconds: aDecoder nextEncodedInt32);
		yourself.
]

{ #category : #'*Fuel-Core' }
DateAndTime >> serializeOn: anEncoder [

	anEncoder 
		encodeUint32: self julianDayNumberUTC;
		encodeUint32: self nanoSecond;
		encodeInt24: self secondsSinceMidnightUTC;
		encodeInt24: self offset asSeconds;
		encodeInt32: self offset nanoSeconds.
]
