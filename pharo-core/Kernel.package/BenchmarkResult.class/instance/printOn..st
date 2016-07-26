printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(.
	self isEmpty
		ifTrue: [ 
			stream << ' empty' ]
		ifFalse: [ 
			iterations printWithCommasOn: stream.
			stream space; << ('iteration' asPluralBasedOn: iterations).
			stream << ' in '.
			elapsedTime printHumanReadableOn: stream.
			stream << '. '.
			self printFrequenceOn: stream ].
	stream nextPut: $)