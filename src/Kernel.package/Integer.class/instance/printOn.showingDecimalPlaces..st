printing
printOn: aStream showingDecimalPlaces: placesDesired
	"Same as super, but provides a faster implementation because fraction part and rounding are trivial."
	
	self printOn: aStream base: 10.
	
	placesDesired <= 0 ifTrue: [ ^ self ].
	
	aStream nextPut: $..
	placesDesired timesRepeat: [ aStream nextPut: $0 ]