tests
testAsSeconds

	| secondsSinceEpoch dateUTC dateEDT datePST |
	self useTimeZone: 'UTC' during: [
		dateUTC := Date readFrom: '01-23-2004' readStream.
		secondsSinceEpoch := (dateUTC start - DateAndTime epoch) asSeconds.
		self assert: dateUTC asSeconds equals: secondsSinceEpoch.
		self assert: (Date fromSeconds: dateUTC asSeconds) equals: dateUTC ].
	
	self useTimeZone: 'EDT' during: [
		dateEDT := Date readFrom: '01-23-2004' readStream.
		secondsSinceEpoch := (dateEDT start - DateAndTime epoch) asSeconds.
		self assert: dateEDT asSeconds equals: secondsSinceEpoch.
		self assert: (Date fromSeconds: dateEDT asSeconds) equals: dateEDT ].
	
	self useTimeZone: 'PST' during: [
		datePST := Date readFrom: '01-23-2004' readStream.
		secondsSinceEpoch := (datePST start - DateAndTime epoch) asSeconds.
		self assert: datePST asSeconds equals: secondsSinceEpoch.
		self assert: (Date fromSeconds: datePST asSeconds) equals: datePST ].
	
	self assert: dateUTC asSeconds  equals: dateEDT asSeconds - (4*3600).
	self assert: dateUTC asSeconds  equals: datePST asSeconds - (8*3600).	