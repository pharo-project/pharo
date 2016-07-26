printing
printHumanReadableOn: stream
	| outputWritten count |
	outputWritten := false.
	self negative 
		ifTrue: [ 
			stream << '- '. 
			^ self abs printHumanReadableOn: stream ].
	#( (days 'day') 
		(hours 'hour') 
		(minutes 'minute') 
		(seconds 'second') 
		(wholeMilliseconds 'millisecond') 
		(wholeMicroseconds 'microsecond') 
		(wholeNanoseconds 'nanosecond')) do: [ :each |
		count := self perform: each first.
		count > 0 
			ifTrue: [
				outputWritten ifTrue: [ stream space ].				
				count printWithCommasOn: stream.
				stream space; << (each second asPluralBasedOn: count).
				outputWritten := true ] ].
	outputWritten ifFalse: [ stream << '0 seconds' ]