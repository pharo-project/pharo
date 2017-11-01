"
This represents a particular point in time during any given day.  For example, '5:19:45 pm'.

If you need a point in time on a particular day, use DateAndTime.  If you need a duration of time, use Duration.

"
Class {
	#name : #Time,
	#superclass : #Magnitude,
	#instVars : [
		'seconds',
		'nanos'
	],
	#pools : [
		'ChronologyConstants'
	],
	#category : 'Kernel-Chronology'
}

{ #category : #benchmarks }
Time class >> benchmarkMillisecondClock [		"Time benchmarkMillisecondClock"
	"Benchmark the time spent in a call to Time>>millisecondClockValue.
	On the VM level this tests the efficiency of calls to ioMSecs()."
	"PII/400 Windows 98: 0.725 microseconds per call"
	| temp1 temp2 temp3 delayTime nLoops time |
	delayTime := 5000. "Time to run benchmark is approx. 2*delayTime"

	"Don't run the benchmark if we have an active delay since
	we will measure the additional penalty in the primitive dispatch
	mechanism (see #benchmarkPrimitiveResponseDelay)."
	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	"Flush the cache for this benchmark so we will have
	a clear cache hit for each send to #millisecondClockValue below"
	Object flushCache.
	temp1 := 0.
	temp2 := self. "e.g., temp1 == Time"
	temp3 := self millisecondClockValue + delayTime.

	"Now check how often we can run the following loop in the given time"
	[temp2 millisecondClockValue < temp3]
		whileTrue:[temp1 := temp1 + 1].

	nLoops := temp1. "Remember the loops we have run during delayTime"

	"Setup the second loop"
	temp1 := 0.
	temp3 := nLoops.

	"Now measure how much time we spend without sending #millisecondClockValue"
	time := Time millisecondClockValue.
	[temp1 < temp3]
		whileTrue:[temp1 := temp1 + 1].
	time := Time millisecondsSince: time.

	"And compute the number of microseconds spent per call to #millisecondClockValue"
	^((delayTime - time * 1000.0 / nLoops) truncateTo: 0.001) printString,
		' microseconds per call to Time>>millisecondClockValue'
]

{ #category : #benchmarks }
Time class >> benchmarkPrimitiveResponseDelay [	"Time benchmarkPrimitiveResponseDelay"
	"Benchmark the overhead for primitive dispatches with an active Delay.
	On the VM level, this tests the efficiency of ioLowResMSecs."

	"PII/400 Windows98: 0.128 microseconds per prim"

	"ar 9/6/1999: This value is *extremely* important for stuff like sockets etc.
	I had a bad surprise when Michael pointed this particular problem out:
	Using the hardcoded clock() call for ioLowResMSecs on Win32 resulted in an overhead
	of 157.4 microseconds per primitive call - meaning you can't get no more than
	approx. 6000 primitives per second on my 400Mhz PII system with an active delay!
	BTW, it finally explains why Squeak seemed soooo slow when running PWS or 
	other socket stuff. The new version (not using clock() but some Windows function) 
	looks a lot better (see above; approx. 8,000,000 prims per sec with an active delay)."

	| nLoops bb index baseTime actualTime delayTime |
	delayTime := 5000. "Time to run this test is approx. 3*delayTime"

	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	bb := Array new: 1. "The object we send the prim message to"

	"Compute the # of loops we'll run in a decent amount of time"
	[(Delay forMilliseconds: delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	nLoops := 0.
	[Delay anyActive] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		nLoops := nLoops + 1.
	].

	"Flush the cache and make sure #basicSize is in there"
	Object flushCache.
	bb basicSize.

	"Now run the loop without any active delay
	for getting an idea about its actual speed."
	baseTime := self millisecondClockValue.
	index := nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index := index - 1.
	].
	baseTime := self millisecondsSince: baseTime.

	"Setup the active delay but try to never make it active"
	[(Delay forMilliseconds: delayTime + delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	"And run the loop"
	actualTime := self millisecondClockValue.
	index := nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index := index - 1.
	].
	actualTime := self millisecondsSince: actualTime.

	"And get us some result"
	^((actualTime - baseTime) * 1000 asFloat / (nLoops * 10) truncateTo: 0.001) printString,
		' microseconds overhead per primitive call'
]

{ #category : #'instance creation' }
Time class >> current [ 

	^ self now
]

{ #category : #accessing }
Time class >> dateAndTimeFromSeconds: secondCountSinceEpoch [
 
	| dateAndTime |
	dateAndTime := DateAndTime fromSeconds: secondCountSinceEpoch.
 	^ Array
 		with: dateAndTime asDate
 		with: dateAndTime asTime
]

{ #category : #'instance creation' }
Time class >> fromSeconds: secondCount [
	"Answer an instance of me that is secondCount number of seconds since midnight."

	| integerSeconds nanos |
	integerSeconds := secondCount truncated.
	nanos := integerSeconds = secondCount
		ifTrue: [ 0 ]
		ifFalse: [ ((secondCount - integerSeconds) * NanosInSecond) asInteger ].
	^ self seconds: integerSeconds nanoSeconds: nanos
]

{ #category : #'instance creation' }
Time class >> fromString: aString [
	^ self readFrom: aString readStream.

]

{ #category : #'instance creation' }
Time class >> hour: hour minute: minute second: second [
	"Answer a Time"
 
 	^ self hour: hour minute: minute second: second nanoSecond: 0
]

{ #category : #'instance creation' }
Time class >> hour: hour minute: minute second: second  nano: nano [
 
	| nanoMultiplicative |
	
	nanoMultiplicative := 1000000000 / (10 ** nano numberOfDigits).	
	
 	^ self hour: hour minute: minute second: second nanoSecond: nano * nanoMultiplicative
]

{ #category : #'instance creation' }
Time class >> hour: hour minute: minute second: second  nanoSecond: nanoCount [
	"Answer a Time - only second precision for now"
 
 	^ self 
		seconds: (hour * SecondsInHour) + (minute * SecondsInMinute) + second 
		nanoSeconds: nanoCount
]

{ #category : #primitives }
Time class >> microsecondClockValue [
	"Answer the number of microseconds since Squeak Epoch"

	^self primUTCMicrosecondsClock 
]

{ #category : #accessing }
Time class >> microsecondsSince: lastTimeInMicroSeconds [
 	"Answer the elapsed time since last recorded in microseconds"
 
 	^self microsecondClockValue - lastTimeInMicroSeconds
]

{ #category : #'general inquiries' }
Time class >> microsecondsToRun: timedBlock [ 
	"Answer the number of milliseconds timedBlock takes to return its value."

	| initialMicroseconds |
	initialMicroseconds := self microsecondClockValue.
	timedBlock value.
	^self microsecondClockValue - initialMicroseconds
]

{ #category : #'instance creation' }
Time class >> midnight [

	^ self seconds: 0

]

{ #category : #primitives }
Time class >> millisecondClockValue [
	"Answer the number of milliseconds since the millisecond clock was last reset or rolled over.
	Answer 0 if the primitive fails."

	<primitive: 135>
	^ 0
]

{ #category : #accessing }
Time class >> milliseconds: currentTime since: lastTime [
	"Answer the elapsed time since last recorded in milliseconds.
	Compensate for rollover."

	| delta |
	delta := currentTime - lastTime.
	^ delta < 0
		ifTrue: [SmallInteger maxVal + delta]
		ifFalse: [delta]

]

{ #category : #accessing }
Time class >> millisecondsSince: lastTime [
 	"Answer the elapsed time since last recorded in milliseconds.
 	Compensate for rollover."
 
 	^self milliseconds: self millisecondClockValue since: lastTime
]

{ #category : #'general inquiries' }
Time class >> millisecondsToRun: timedBlock [ 
	"Answer the number of milliseconds timedBlock takes to return its value."

	^(self microsecondsToRun: timedBlock) // 1e3
]

{ #category : #'instance creation' }
Time class >> new [
	"Answer a Time representing midnight"

	^ self midnight
]

{ #category : #'instance creation' }
Time class >> noon [

	^ self seconds: (SecondsInDay / 2)

]

{ #category : #'instance creation' }
Time class >> now [
	"Answer a Time representing the time right now - this is a 24 hour clock."
	
	^ self nowLocal
]

{ #category : #'instance creation' }
Time class >> nowLocal [
	"Answer the time since midnight in local timezone"

	| microSecondsToday |
	microSecondsToday := self primUTCMicrosecondsClock.
	^ self
		seconds: microSecondsToday // 1000000 + DateAndTime localTimeZone offset asSeconds \\ SecondsInDay
		nanoSeconds: microSecondsToday \\ 1000000 * 1000
]

{ #category : #'instance creation' }
Time class >> nowUTC [
	"Answer the time since midnight in UTC timezone"

	| microSecondsToday |
	microSecondsToday := self primUTCMicrosecondsClock \\ MicrosecondsInDay.
	^ self 
		seconds: microSecondsToday // 1000000 
		nanoSeconds: microSecondsToday \\ 1000000 * 1000
]

{ #category : #primitives }
Time class >> primMillisecondClock [
	"Primitive. Answer the number of milliseconds since the millisecond clock
	 was last reset or rolled over. Answer zero if the primitive fails.
	As an alternative you can use #primUTCMillisecondsClock which does not overflow."

	<primitive: 135>
	^ 0
]

{ #category : #primitives }
Time class >> primUTCMicrosecondsClock [
	"Answer the number of micro-seconds ellapsed since Squeak epoch.
	That is since 00:00 on the morning of January 1, 1901 UTC.
	At least a 60-bit unsigned integer is used internally which is enough for dates up to year 38435.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 240>
	self primitiveFailed
]

{ #category : #primitives }
Time class >> primUTCMillisecondsClock [
	"Answer the number of whole milliseconds ellapsed since Squeak epoch.
	That is since 00:00 on the morning of January 1, 1901 UTC"

	^self primUTCMicrosecondsClock // 1e3
]

{ #category : #primitives }
Time class >> primUTCSecondsClock [
	"Answer the number of whole seconds ellapsed since Squeak epoch.
	That is since 00:00 on the morning of January 1, 1901 UTC"

	^self primUTCMicrosecondsClock // 1e6
]

{ #category : #'instance creation' }
Time class >> readFrom: aStream [
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second>.<nseconds> <am/pm>
	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm nanos power |
	hour := Integer readFrom: aStream.
	minute := second := nanos := 0.
	(aStream peekFor: $:) 
		ifTrue: [
			minute := Integer readFrom: aStream.
			(aStream peekFor: $:) 
				ifTrue: [
					second := Integer readFrom: aStream.
					(aStream peekFor: $.)
						ifTrue: [ 
							power := 1.
							[ aStream atEnd not and: [ aStream peek isDigit ] ]
								whileTrue: [  
									nanos := nanos * 10 + aStream next digitValue.
									power := power * 10 ].
							nanos := nanos / power * 1000000000 ] ] ].
	aStream skipSeparators.
	(aStream atEnd not and: [ 'APap' includes: aStream peek ]) 
		ifTrue: [
			ampm := aStream next asLowercase.
			(ampm = $p and: [ hour < 12 ]) ifTrue: [ hour := hour + 12 ].
			(ampm = $a and: [ hour = 12 ]) ifTrue: [ hour := 0 ].
			(aStream peekFor: $m) ifFalse: [ aStream peekFor: $M ] ].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanos
]

{ #category : #'instance creation' }
Time class >> seconds: seconds [
	"Answer a Time from midnight."

	^ self basicNew ticks: (Duration seconds: seconds) ticks
]

{ #category : #'instance creation' }
Time class >> seconds: seconds nanoSeconds: nanoCount [
	"Answer a Time from midnight."

	^ self basicNew
		ticks: (Duration seconds: seconds nanoSeconds: nanoCount) ticks
]

{ #category : #clock }
Time class >> secondsWhenClockTicks [

	"waits for the moment when a new second begins"

	| lastSecond |

	lastSecond := self primUTCSecondsClock.
	[ lastSecond = self primUTCSecondsClock ] whileTrue: [ (Delay forMilliseconds: 1) wait ].
 
	^ lastSecond + 1 
]

{ #category : #accessing }
Time class >> totalSeconds [
	"Answer the total seconds ellapsed since the Squeak epoch: 1 January 1901 00:00 UTC"

	^ self primUTCSecondsClock
]

{ #category : #comparing }
Time >> < aTime [

	^ self asDuration < aTime asDuration
]

{ #category : #comparing }
Time >> = aTime [

	^ [ self ticks = aTime ticks ]
		on: MessageNotUnderstood do: [false]
]

{ #category : #adding }
Time >> addSeconds: nSeconds [ 
	"Answer a Time that is nSeconds after the receiver."

	^ self class seconds: self asSeconds + nSeconds
]

{ #category : #adding }
Time >> addTime: timeAmount [
 	"Answer a Time that is timeInterval after the receiver. timeInterval is an 
 	instance of Date or Time."
 
 	^ self class seconds: self asSeconds + timeAmount asSeconds
]

{ #category : #converting }
Time >> asDate [

	^ Date today
]

{ #category : #converting }
Time >> asDateAndTime [

	^ DateAndTime today + self
]

{ #category : #converting }
Time >> asDuration [ 
	"Answer the duration since midnight"

	^ Duration seconds: seconds nanoSeconds: nanos 
]

{ #category : #converting }
Time >> asMilliSeconds [
	"Answer the number of milliseconds since midnight"

	^ self asDuration asMilliSeconds 
]

{ #category : #converting }
Time >> asMonth [

	^ self asDateAndTime asMonth
]

{ #category : #converting }
Time >> asNanoSeconds [
	"Answer the number of nanoseconds since midnight"

	^ self asDuration asNanoSeconds 
]

{ #category : #converting }
Time >> asSeconds [
 	"Answer the number of seconds since midnight of the receiver."
 
 	^ seconds
]

{ #category : #converting }
Time >> asTime [

	^ self
]

{ #category : #converting }
Time >> asWeek [

	^ self asDateAndTime asWeek
]

{ #category : #converting }
Time >> asYear [

	^ self asDateAndTime asYear
]

{ #category : #accessing }
Time >> duration [

	^ Duration zero 
]

{ #category : #comparing }
Time >> hash [

	^ self ticks hash 
]

{ #category : #printing }
Time >> hhmm24 [
 	"Return a string of the form 1123 (for 11:23 am), 2154 (for 9:54 pm), of exactly 4 digits"
 
 	^ String new: 4 streamContents: [ :aStream | 
		self hour printOn: aStream base: 10 length: 2 padded: true.
		self minute printOn: aStream base: 10 length: 2 padded: true ]
]

{ #category : #accessing }
Time >> hour [

	^ self hour24 
]

{ #category : #accessing }
Time >> hour12 [
 	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
 	of the day in the 12-hour clock of the local time of the receiver."
	^ self hour24 - 1 \\ 12 + 1
]

{ #category : #accessing }
Time >> hour24 [ 
	"Answer a number that represents the number of complete hours in the receiver, 
	after the number of complete days has been removed."
 
 	^ (seconds rem: SecondsInDay) quo: SecondsInHour
]

{ #category : #accessing }
Time >> hours [

	^ self hour
]

{ #category : #printing }
Time >> intervalString [
	"Treat the time as a difference.  Give it in hours and minutes with two digits of accuracy."

	| d |
	d := self asDuration.
	^ String streamContents: [ :s |
		d hours > 0 ifTrue: [s print: d hours; nextPutAll: ' hours'].
		d minutes > 0 ifTrue: [s space; print: d minutes; nextPutAll: ' minutes'].
		d seconds > 0 ifTrue: [s space; print: d seconds; nextPutAll: ' seconds'] ].


]

{ #category : #accessing }
Time >> meridianAbbreviation [

	^ self hour < 12 ifTrue: ['AM'] ifFalse: ['PM']. 
]

{ #category : #accessing }
Time >> minute [
	"Answer a number that represents the number of complete minutes in the receiver,
	after the number of complete hours has been removed."
 
	^ (seconds rem: SecondsInHour) quo: SecondsInMinute 
]

{ #category : #accessing }
Time >> minutes [

	^ self asDuration minutes
]

{ #category : #accessing }
Time >> nanoSecond [ 

	^ nanos 
]

{ #category : #printing }
Time >> print24 [
 	"Return as 8-digit string 'hh:mm:ss', with leading zeros if needed"
 
 	^ String new: 8 streamContents: [ :aStream | 
		self print24: true on: aStream ]
 
 
]

{ #category : #printing }
Time >> print24: hr24 on: aStream [ 
 	"Format is 'hh:mm:ss' or 'h:mm:ss am' "
 
 	| h m s |
	h := self hour. m := self minute. s := self second.
	hr24
		ifTrue: [ 
			h < 10 ifTrue: [ aStream nextPut: $0 ].
			h printOn: aStream ]
		ifFalse: [ 
			h > 12
				ifTrue: [ h - 12 printOn: aStream ]
				ifFalse: [
					h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ] ] ].
	aStream nextPutAll: (m < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
	m printOn: aStream.
	aStream nextPutAll: (s < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
	s printOn: aStream.
	hr24 ifFalse: [ aStream nextPutAll: (h < 12 ifTrue: [ ' am' ] ifFalse: [ ' pm' ]) ]
]

{ #category : #printing }
Time >> print24: hr24 showSeconds: showSeconds on: aStream [ 
	"Format is 'hh:mm:ss.nnnnnnnnn' or 'h:mm:ss.nnnnnnnnn am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h := self hour. m := self minute. s := self second.
	hr24
		ifTrue: [ 
			h < 10 ifTrue: [ aStream nextPut: $0 ].
			h printOn: aStream ]
		ifFalse: [ 
			h > 12
				ifTrue: [ h - 12 printOn: aStream ]
				ifFalse: [
					h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ] ] ].
	aStream nextPutAll: (m < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
	m printOn: aStream.
	showSeconds 
		ifTrue: [
			aStream nextPutAll: (s < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
			s printOn: aStream.
			nanos = 0
				ifFalse: [ | n len |
					n := nanos. len := 9.
					[ n \\ 10 = 0 ] whileTrue: [ n := n / 10. len := len - 1 ].
					aStream nextPut: $..
					n printOn: aStream base: 10 length: len padded: true ] ].
	hr24 ifFalse: [ aStream nextPutAll: (h < 12 ifTrue: [ ' am' ] ifFalse: [ ' pm' ]) ]
]

{ #category : #printing }
Time >> printMinutes [
 	"Return as string 'hh:mm pm'  "
 
 	^ String new: 8 streamContents: [ :aStream | 
		self print24: false showSeconds: false on: aStream ]
 
]

{ #category : #printing }
Time >> printOn: aStream [ 

	self 
		print24: false
		showSeconds: (self seconds ~= 0 or: [ self nanoSecond ~= 0 ])
		on: aStream
]

{ #category : #accessing }
Time >> second [ 
	"Answer a number that represents the number of complete seconds in the receiver, 
	after the number of complete minutes has been removed."
 
 	^ (seconds rem: SecondsInMinute)
]

{ #category : #accessing }
Time >> seconds [

	^ self second
]

{ #category : #private }
Time >> seconds: secondCount [
	"Private - only used by Time class."

	seconds := secondCount.
	nanos := 0
]

{ #category : #private }
Time >> seconds: secondCount nanoSeconds: nanoCount [ 
	"Private - only used by Time class."

	seconds := secondCount.
	nanos := nanoCount
]

{ #category : #printing }
Time >> storeOn: aStream [
 
 	aStream print: self printString; nextPutAll: ' asTime'
]

{ #category : #adding }
Time >> subtractTime: timeAmount [ 
	"Answer a Time that is timeInterval before the receiver. timeInterval is  
	an instance of Date or Time."

	^ self class seconds: self asSeconds - timeAmount asSeconds
]

{ #category : #accessing }
Time >> ticks [
	"Answer an Array: { seconds. nanoSeconds }"

	^ Array with: 0 with: seconds with: nanos.
]

{ #category : #private }
Time >> ticks: anArray [
	"ticks is an Array: { days. seconds. nanoSeconds }"

	seconds := anArray at: 2.
	nanos := anArray at: 3
]

{ #category : #accessing }
Time >> to: anEnd [
	"Answer a Timespan. anEnd must respond to #asDateAndTime"

	^ self asDateAndTime to: anEnd
]
