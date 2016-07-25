input
fuzzyReadFrom: aStream
	| bc year month day hour minute second nanos offset buffer ch |


	aStream peek = $- ifTrue: [ aStream next. bc := -1] ifFalse: [bc := 1].
	year := (aStream upTo: $-) asInteger * bc.
	month := (aStream upTo: $-) asInteger ifNil: [1].
	day := (aStream upTo: $T) asInteger ifNil: [1].
	hour := (aStream upTo: $:) asInteger ifNil: [0].
 	buffer := '00:' copy. ch := nil.
	minute := buffer writeStream.
	[ aStream atEnd | (ch = $:) | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch := minute nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch := $: ].
	minute := (buffer readStream upTo: ch) asInteger.
	buffer := '00.' copy.
	second := buffer writeStream.
	[ aStream atEnd | (ch = $.) | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch := second nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch := $. ].
	second := (buffer readStream upTo: ch) asInteger.
	buffer := '000000000' copy.
	(ch = $.) ifTrue: [ 
		nanos := buffer writeStream.
		[ aStream atEnd | ((ch := aStream next) = $+) | (ch = $-) ]
			whileFalse: [ nanos nextPut: ch. ].
		(ch isNil or: [ch isDigit]) ifTrue: [ ch := $+ ].
	].

	nanos := buffer asInteger.
	aStream atEnd
		ifTrue: [ offset := Duration zero ]
		ifFalse: [ch := aStream next.
                       ch = $+ ifTrue: [ch := Character space].
                       offset := Duration fromString: ch asString, '0:', aStream upToEnd, ':0'].
	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute

		second: second
		nanoSecond:  nanos

		offset: offset.
