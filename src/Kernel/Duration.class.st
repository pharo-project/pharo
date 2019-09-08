"
I represent a duration of time. I have nanosecond precision
"
Class {
	#name : #Duration,
	#superclass : #Magnitude,
	#instVars : [
		'nanos',
		'seconds'
	],
	#pools : [
		'ChronologyConstants'
	],
	#category : #'Kernel-Chronology'
}

{ #category : #'instance creation simple' }
Duration class >> days: aNumber [

	^ self seconds: aNumber * SecondsInDay nanoSeconds: 0
]

{ #category : #'instance creation' }
Duration class >> days: days hours: hours minutes: minutes seconds: seconds [

	^ self days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: 0
]

{ #category : #'instance creation' }
Duration class >> days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos [	

 	^ self seconds: ((days * SecondsInDay) 
						+ (hours * SecondsInHour)
							+ (minutes * SecondsInMinute) 
								+ seconds)
		nanoSeconds: nanos

]

{ #category : #'instance creation' }
Duration class >> days: days seconds: seconds [

	^ self basicNew seconds: days * SecondsInDay + seconds nanoSeconds: 0

]

{ #category : #'instance creation' }
Duration class >> fromString: aString [

	^ self readFrom: aString readStream

]

{ #category : #'instance creation simple' }
Duration class >> hours: aNumber [

	^ self seconds: aNumber * SecondsInHour nanoSeconds: 0
]

{ #category : #'instance creation simple' }
Duration class >> milliSeconds: milliCount [
     
	^ self
		seconds: (milliCount quo: 1000)
		nanoSeconds: (milliCount rem: 1000) * NanosInMillisecond
]

{ #category : #'instance creation simple' }
Duration class >> minutes: aNumber [

	^ self seconds: aNumber * SecondsInMinute nanoSeconds: 0
]

{ #category : #'instance creation simple' }
Duration class >> nanoSeconds: nanos [
	"This method is slow. If you have nanos less than 10^6 you should use #seconds:nanoSeconds: instead."

	| quo |
	quo := nanos quo: NanosInSecond.
	^ self basicNew
		seconds: quo
		nanoSeconds: nanos - (quo * NanosInSecond)
]

{ #category : #'instance creation' }
Duration class >> readFrom: aStream [
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"

	| sign days hours minutes seconds nanos nanosBuffer |
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	days := (aStream upTo: $:) asInteger sign: sign.
	hours := (aStream upTo: $:) asInteger sign: sign.
	minutes := (aStream upTo: $:) asInteger sign: sign.
	seconds := (aStream upTo: $.) asInteger sign: sign.
	nanosBuffer := '000000000' copy.
	nanos := nanosBuffer writeStream.
	[aStream atEnd not and: [aStream peek isDigit]]
		whileTrue: [nanos nextPut: aStream next].
		
	^ self 
		days: days 
		hours: hours 
		minutes: minutes 
		seconds: seconds 
		nanoSeconds: (nanosBuffer asInteger sign: sign)
]

{ #category : #'instance creation simple' }
Duration class >> seconds: seconds [

	^ self seconds: seconds nanoSeconds: 0

]

{ #category : #'instance creation' }
Duration class >> seconds: seconds nanoSeconds: nanos [
	^ self basicNew
		seconds: seconds truncated
		nanoSeconds: seconds fractionPart * NanosInSecond + nanos
]

{ #category : #'instance creation simple' }
Duration class >> weeks: aNumber [

	^ self days: (aNumber * 7) seconds: 0

]

{ #category : #'instance creation simple' }
Duration class >> years: aNumber [

	^ self days: (aNumber * 365) seconds: 0

]

{ #category : #'instance creation simple' }
Duration class >> zero [

	^ self basicNew seconds: 0 nanoSeconds: 0

]

{ #category : #arithmetic }
Duration >> * operand [
	"operand is a Number"
	^ self class nanoSeconds: ( (self asNanoSeconds * operand) asInteger)

]

{ #category : #arithmetic }
Duration >> + operand [
  	"operand is a Duration"
	^ self class nanoSeconds: (self asNanoSeconds + operand asNanoSeconds)
 
]

{ #category : #arithmetic }
Duration >> - operand [
	"operand is a Duration"
	^ self + operand negated
 
]

{ #category : #arithmetic }
Duration >> / operand [
 	"operand is a Duration or a Number"
 
 	^ operand isNumber
 		ifTrue: [ self class nanoSeconds: (self asNanoSeconds / operand) asInteger ]
 		ifFalse: [ self asNanoSeconds / operand asDuration asNanoSeconds ]
 
 
]

{ #category : #operations }
Duration >> // operand [
 
 	"operand is a Duration or a Number"
 
 
 	^ operand isNumber
 		ifTrue: [ self class nanoSeconds: (self asNanoSeconds // operand) asInteger ]
 		ifFalse: [ self asNanoSeconds // operand asDuration asNanoSeconds ]
 
]

{ #category : #arithmetic }
Duration >> < comparand [
 
 	^ self asNanoSeconds < comparand asNanoSeconds
 
]

{ #category : #comparing }
Duration >> = comparand [ 
 	"Answer whether the argument is a <Duration> representing the same 
 	period of time as the receiver."
 
 	^ self == comparand
 		ifTrue: [true]
 		ifFalse: 
 			[self species = comparand species 
 				ifTrue: [self asNanoSeconds = comparand asNanoSeconds]
 				ifFalse: [false] ]
]

{ #category : #operations }
Duration >> \\ operand [
 
 	"modulo. Remainder defined in terms of //. Answer a Duration with the 
 	same sign as aDuration. operand is a Duration or a Number."
 
 	^ operand isNumber
 		ifTrue: [ self class nanoSeconds: (self asNanoSeconds \\ operand) ]
 		ifFalse: [ self - (operand * (self // operand)) ]
 
]

{ #category : #accessing }
Duration >> abs [
 
 	^ self class seconds: seconds abs nanoSeconds: nanos abs
 
]

{ #category : #converting }
Duration >> asDays [
 	"Answer the number of days in the receiver."
 	^ self asHours / 24
]

{ #category : #converting }
Duration >> asDelay [
 
 	^ Delay forDuration: self
]

{ #category : #converting }
Duration >> asDuration [
 
 	^ self
 
]

{ #category : #converting }
Duration >> asHours [
 	"Answer the number of hours in the receiver."
 	^ self asMinutes / 60.0
]

{ #category : #converting }
Duration >> asMicroseconds [
 
 
 	^ ((seconds * NanosInSecond) + nanos) // (10 raisedToInteger: 3)
 
]

{ #category : #converting }
Duration >> asMilliSeconds [
 
 
 	^ ((seconds * NanosInSecond) + nanos) // (10 raisedToInteger: 6)
 
]

{ #category : #converting }
Duration >> asMinutes [
 	"Answer the number of minutes in the receiver."
 	^ self asNanoSeconds  / 60000000000.0
]

{ #category : #converting }
Duration >> asNanoSeconds [
 
 	^ (seconds * NanosInSecond) + nanos
 
]

{ #category : #converting }
Duration >> asSeconds [
 	"Answer the number of seconds in the receiver."
 	^ seconds
 
]

{ #category : #accessing }
Duration >> days [
 	"Answer a number that represents the number of complete days in the receiver"
	^ seconds quo: SecondsInDay

]

{ #category : #comparing }
Duration >> hash [
	^seconds bitXor: nanos
 
]

{ #category : #accessing }
Duration >> hours [
 	"Answer a number that represents the number of complete hours in the receiver, after the number of complete days has been removed."
 
 	^ (seconds rem: SecondsInDay) quo: SecondsInHour
 
]

{ #category : #printing }
Duration >> humanReadablePrintString [
	"Return a String with a human readable representation of me"

	"99999 atRandom seconds humanReadablePrintString"
	"99999999 atRandom milliSeconds humanReadablePrintString"	
	"99999999999999 atRandom nanoSeconds humanReadablePrintString"
	
	^ String streamContents: [ :out | 
		self printHumanReadableOn: out ]
]

{ #category : #initialization }
Duration >> initialize [
	super initialize.
	self seconds: 0 nanoSeconds: 0.

]

{ #category : #testing }
Duration >> isZero [

	^ seconds = 0 and: [ nanos = 0 ]

]

{ #category : #accessing }
Duration >> minutes [
  	"Answer a number that represents the number of complete minutes in the receiver, after the number of complete hours has been removed."
 	^ (seconds rem: SecondsInHour) quo: SecondsInMinute
 
]

{ #category : #converting }
Duration >> nanoSeconds [
 
 
 	^ nanos
 
]

{ #category : #accessing }
Duration >> negated [
 
 	^ self class seconds: seconds negated nanoSeconds: nanos negated
 
]

{ #category : #accessing }
Duration >> negative [
 
 
 	^ self positive not
 
]

{ #category : #accessing }
Duration >> positive [
 
 
 	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]
 
]

{ #category : #printing }
Duration >> printHumanReadableOn: stream [
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
		count := (self perform: each first) truncated.
		count > 0 
			ifTrue: [
				outputWritten ifTrue: [ stream space ].				
				count printWithCommasOn: stream.
				stream space; << (each second asPluralBasedOn: count).
				outputWritten := true ] ].
	outputWritten ifFalse: [ stream << '0 seconds' ]
]

{ #category : #printing }
Duration >> printOn: aStream [
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]" 	| d h m s n |
	d := self days abs.
	h := self hours abs.
	m := self minutes abs.
 	s := self seconds abs truncated.
	n := self nanoSeconds abs. 	
	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h < 10 ifTrue: [ aStream nextPut: $0. ].
	h printOn: aStream. aStream nextPut: $:.
	m < 10 ifTrue: [ aStream nextPut: $0. ].
	m printOn: aStream. aStream nextPut: $:.
	s < 10 ifTrue: [ aStream nextPut: $0. ].
	s printOn: aStream.
	n = 0 ifFalse:
		[ | z ps |
		aStream nextPut: $..
		ps := n printString padLeftTo: 9 with: $0. 
		z := ps findLast: [ :c | c asciiValue > $0 asciiValue ].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].

]

{ #category : #operations }
Duration >> roundTo: aDuration [
 	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes."
 
 	^ self class nanoSeconds: (self asNanoSeconds roundTo: aDuration asNanoSeconds)
 
 
]

{ #category : #accessing }
Duration >> seconds [
 	"Answer a number that represents the number of complete seconds in the receiver, after the number of complete minutes has been removed."
	
	"1002003004 nanoSeconds seconds"
 
 	^ (seconds rem: SecondsInMinute)
]

{ #category : #private }
Duration >> seconds: secondCount nanoSeconds: nanoCount [ 
	"Private - only used by Duration class"

	seconds := secondCount.
	nanos := nanoCount rounded.
	"normalize if signs do not match"
	[ nanos < 0 and: [ seconds > 0 ] ]
		whileTrue: [ seconds := seconds - 1.
			nanos := nanos + NanosInSecond ].
	[ seconds < 0 and: [ nanos > 0 ] ]
		whileTrue: [ seconds := seconds + 1.
			nanos := nanos - NanosInSecond ]

]

{ #category : #private }
Duration >> storeOn: aStream [
 
 	aStream
 		nextPut: $(;
 		nextPutAll: self className;
 		nextPutAll: ' seconds: ';
 		print: seconds;
 		nextPutAll: ' nanoSeconds: ';
 		print: nanos;
 		nextPut: $).
 
]

{ #category : #private }
Duration >> ticks [
	"Answer an array {days. seconds. nanoSeconds}. Used by DateAndTime and Time."

	| days |
	days := self days.
	^ Array 
		with: days
		with: seconds - (days * SecondsInDay)
		with: nanos
]

{ #category : #accessing }
Duration >> totalSeconds [
	"Return the total number of seconds in me.
	If I have zero nano seconds, the result is equivalent to #seconds. 
	If I have nanoseconds, the result will contain fractional seconds."
	
	"1500 milliSeconds totalSeconds"

	^ nanos = 0
		ifTrue: [ seconds ]
		ifFalse: [ self asNanoSeconds / 1e9 ]
]

{ #category : #operations }
Duration >> truncateTo: aDuration [
 	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 4 minutes."
 
 	^ self class
 		nanoSeconds: (self asNanoSeconds truncateTo: aDuration asNanoSeconds)
 
 
]

{ #category : #operations }
Duration >> wait [

	self asDelay wait
]

{ #category : #accessing }
Duration >> wholeMicroseconds [
	"Answer the number of whole microseconds in me after whole seconds and milliseconds are removed"
	
	"1002003004 nanoSeconds wholeMicroseconds"

	^ (nanos rem: 1e6) quo: 1e3
]

{ #category : #accessing }
Duration >> wholeMilliseconds [
	"Answer the number of whole milliseconds in me after whole seconds are removed"

	"1002003004 nanoSeconds wholeMilliseconds"
	
	^ nanos quo: 1e6
]

{ #category : #accessing }
Duration >> wholeNanoseconds [
	"Answer the number of whole nanoseconds in me after whole seconds, milli & microseconds are removed"

	"1002003004 nanoSeconds wholeNanoseconds"

	^ nanos rem: 1e3
]
