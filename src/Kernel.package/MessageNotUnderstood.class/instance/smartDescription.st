accessing
smartDescription
	
	message ifNil: [^self description].

	message lookupClass == UndefinedObject ifTrue: [ 
		^message selector printString, ' was sent to nil'].

	^'Instance of ', message lookupClass printString
		, ' did not understand ', message selector printString