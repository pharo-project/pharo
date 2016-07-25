parsing
parseInt

	| negate num |

	negate := false.
	(stream peek = $-) ifTrue: [ 
		negate := true. 
		stream next.
		self skipSpace ].

	num := String new writeStream.	
	[ stream atEnd not and: [stream peek isDigit ] ] 
	whileTrue: [ num nextPut: stream next ].

	num := num contents asInteger.
	negate ifTrue: [ num := num negated ].
	
	^ num