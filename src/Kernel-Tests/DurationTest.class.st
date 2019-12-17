"
SUnit tests for class  Duration
"
Class {
	#name : #DurationTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'aDuration'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
DurationTest >> classToBeTested [

	^ Duration



]

{ #category : #coverage }
DurationTest >> selectorsToBeIgnored [

	| private | 
	private := #( #printOn: ).

	^ super selectorsToBeIgnored, private

]

{ #category : #running }
DurationTest >> setUp [
	super setUp.
	aDuration := Duration days: 1 hours: 2 minutes: 3 seconds: 4 nanoSeconds: 5 
]

{ #category : #tests }
DurationTest >> testAbs [
	self assert: aDuration abs equals: aDuration.
	self assert: (Duration nanoSeconds: -5) abs equals: (Duration nanoSeconds: 5)
]

{ #category : #tests }
DurationTest >> testAsDay [
	| full half quarter |
	full := Duration minutes: 60 * 24.
	half := Duration minutes: 60 * 12.
	quarter := Duration minutes: 60 * 6.
	self
		assert: 1 day equals: full;
		assert: 1.0 day equals: full;
		assert: 0.5 day equals: half;
		assert: (1 / 2) day equals: half;
		assert: (1 / 4) day equals: quarter.
	self assert: 0.4 day + 0.6 day equals: 1 day
]

{ #category : #tests }
DurationTest >> testAsDelay [
	self deny: aDuration asDelay equals: aDuration
	"want to come up with a more meaningful test"
]

{ #category : #tests }
DurationTest >> testAsDuration [
	self assert: aDuration asDuration equals: aDuration
]

{ #category : #tests }
DurationTest >> testAsMilliSecond [
	self
		assert: 1 milliSecond equals: (1 / 1000) second;
		assert: (1 / 2) milliSecond equals: (1 / 2000) second;
		assert: 0.5 milliSecond equals: (1 / 2000) second;
		assert: 500 milliSecond equals: (1 / 2) second
]

{ #category : #tests }
DurationTest >> testAsMilliSeconds [
	self assert: (Duration nanoSeconds: 1000000) asMilliSeconds equals: 1.
	self assert: (Duration seconds: 1) asMilliSeconds equals: 1000.
	self assert: (Duration nanoSeconds: 1000000) asMilliSeconds equals: 1.
	self assert: (Duration nanoSeconds: 1000000) asMilliSeconds equals: 1.
	self assert: aDuration asMilliSeconds equals: 93784000.
	self assert: (Duration milliSeconds: 3775) asSeconds equals: 3.
	self assert: (Duration milliSeconds: 3775) nanoSeconds equals: 775000000.
	self assert: (Duration milliSeconds: -3775) asSeconds equals: -3.
	self assert: (Duration milliSeconds: -3775) nanoSeconds equals: -775000000
]

{ #category : #tests }
DurationTest >> testAsMinute [
	| full half quarter |
	full := Duration seconds: 60.
	half := Duration seconds: 30.
	quarter := Duration seconds: 15.
	self
		assert: 1 minute equals: full;
		assert: 1.0 minute equals: full;
		assert: 0.5 minute equals: half;
		assert: (1 / 2) minute equals: half;
		assert: (1 / 4) minute equals: quarter.
	self assert: 0.4 minute + 0.6 minute equals: 1 minute
]

{ #category : #tests }
DurationTest >> testAsNanoSeconds [
	self assert: (Duration nanoSeconds: 1) asNanoSeconds equals: 1.
	self assert: (Duration seconds: 1) asNanoSeconds equals: 1000000000.
	self assert: aDuration asNanoSeconds equals: 93784000000005
]

{ #category : #tests }
DurationTest >> testAsSecond [
	| full half quarter |
	full := Duration seconds: 1.
	half := Duration seconds: 0.5.
	quarter := Duration seconds: 0.25.
	self
		assert: 1 second equals: full;
		assert: 1.0 second equals: full;
		assert: 0.5 second equals: half;
		assert: (1 / 2) second equals: half;
		assert: (1 / 4) second equals: quarter.
	self assert: 0.4 second + 0.6 second equals: 1 second
]

{ #category : #tests }
DurationTest >> testAsSeconds [
	self assert: (Duration nanoSeconds: 1000000000) asSeconds equals: 1.
	self assert: (Duration seconds: 1) asSeconds equals: 1.
	self assert: aDuration asSeconds equals: 93784.
	self assert: 1 asSeconds equals: (Duration seconds: 1).
	self assert: (1 / 2) asSeconds equals: (Duration milliSeconds: 500)
]

{ #category : #tests }
DurationTest >> testAsWeek [
	| full half quarter |
	full := Duration days: 7.
	half := Duration weeks: 0.5.
	quarter := Duration weeks: 0.25.
	self
		assert: 1 week equals: full;
		assert: 1.0 week equals: full;
		assert: 0.5 week equals: half;
		assert: (1 / 2) week equals: half;
		assert: (1 / 4) week equals: quarter.
	self assert: 0.4 week + 0.6 week equals: 1 week
]

{ #category : #tests }
DurationTest >> testAsWeeks [
	| full half quarter |
	full := Duration days: 7.
	half := Duration weeks: 0.5.
	quarter := Duration weeks: 0.25.
	self
		assert: 1 weeks equals: full;
		assert: 1.0 weeks equals: full;
		assert: 0.5 weeks equals: half;
		assert: (1 / 2) weeks equals: half;
		assert: (1 / 4) weeks equals: quarter.
	self assert: 1.4 weeks + 1.6 weeks equals: 3 weeks
]

{ #category : #tests }
DurationTest >> testAsYear [
	self
		assert: 1 year days equals: 365;
		assert: 0.5 year asHours equals: 364 / 2 * 24 + 12
]

{ #category : #tests }
DurationTest >> testAsYears [
	self
		assert: 2 years days equals: 730;
		assert: 0.5 year asHours equals: 364 / 2 * 24 + 12
]

{ #category : #tests }
DurationTest >> testComparing [
	| d1 d2 d3 |
	d1 := Duration seconds: 10 nanoSeconds: 1.
	d2 := Duration seconds: 10 nanoSeconds: 1.
	d3 := Duration seconds: 10 nanoSeconds: 2.

	self
		assert: d1 equals: d1;
		assert: d1 equals: d2;
		deny: d1 equals: d3;
		assert: d1 < d3
]

{ #category : #tests }
DurationTest >> testDays [
	self assert: aDuration days equals: 1.
	self assert: (Duration days: 1) days equals: 1
]

{ #category : #tests }
DurationTest >> testDivide [
	self assert: aDuration / aDuration equals: 1.
	self
		assert: aDuration / 2
		equals:
			(Duration
				days: 0
				hours: 13
				minutes: 1
				seconds: 32
				nanoSeconds: 2).
	self
		assert: aDuration / (1 / 2)
		equals:
			(Duration
				days: 2
				hours: 4
				minutes: 6
				seconds: 8
				nanoSeconds: 10)
]

{ #category : #tests }
DurationTest >> testFromString [
	self assert: aDuration equals: (Duration fromString: '1:02:03:04.000000005')
]

{ #category : #tests }
DurationTest >> testHash [
	self
		assert: aDuration hash
		equals:
			(Duration
				days: 1
				hours: 2
				minutes: 3
				seconds: 4
				nanoSeconds: 5) hash
]

{ #category : #tests }
DurationTest >> testHours [
	self assert: aDuration hours equals: 2.
	self assert: (Duration hours: 2) hours equals: 2
]

{ #category : #tests }
DurationTest >> testHumanReadablePrintString [
	self assert: 0 seconds humanReadablePrintString equals: '0 seconds'.
	self assert: 1 second humanReadablePrintString equals: '1 second'.
	self assert: 1 second negated humanReadablePrintString equals: '- 1 second'.
	self 
		assert: 1002003004 nanoSeconds humanReadablePrintString 
		equals: '1 second 2 milliseconds 3 microseconds 4 nanoseconds'.
	self 
		assert: (Duration days: 1 hours: 1 minutes: 1 seconds: 1) humanReadablePrintString 
		equals: '1 day 1 hour 1 minute 1 second'.
	self 
		assert: (Duration days: 2 hours: 4 minutes: 8 seconds: 16) humanReadablePrintString 
		equals: '2 days 4 hours 8 minutes 16 seconds'.
		
	self shouldnt: [ 17.6782 years humanReadablePrintString ] raise: MessageNotUnderstood .
]

{ #category : #tests }
DurationTest >> testIntegerDivision [
	self assert: aDuration // aDuration equals: 1.
	self assert: aDuration // 2 equals: aDuration / 2
	"is there ever a case where this is not true, since precision is always to the nano second?"
]

{ #category : #tests }
DurationTest >> testIsZero [
	self assert: Duration zero isZero.
	self assert: (Duration seconds: 0) isZero	
]

{ #category : #tests }
DurationTest >> testLessThan [
	self assert: aDuration  < (aDuration + 1 day ).
	self deny: aDuration < aDuration.
	
]

{ #category : #tests }
DurationTest >> testMilliSeconds [
	#(#(5 0 5000000) #(1005 1 5000000) #(-5 0 -5000000) #(-1005 -1 -5000000) #(1234567 1234 567000000) #(-1234567 -1234 -567000000))
		do: [ :each | 
			| duration |
			duration := Duration milliSeconds: each first.
			self assert: duration asSeconds equals: each second.
			self assert: duration nanoSeconds equals: each third ]
	"argument (milliseconds)        seconds nanoseconds"
]

{ #category : #tests }
DurationTest >> testMinus [
	self assert: aDuration - aDuration equals: (Duration seconds: 0).
	self
		assert:
			aDuration
				-
					(Duration
						days: -1
						hours: -2
						minutes: -3
						seconds: -4
						nanoSeconds: -5)
		equals:
			(Duration
				days: 2
				hours: 4
				minutes: 6
				seconds: 8
				nanoSeconds: 10).
	self
		assert:
			aDuration
				-
					(Duration
						days: 0
						hours: 1
						minutes: 2
						seconds: 3
						nanoSeconds: 4)
		equals:
			(Duration
				days: 1
				hours: 1
				minutes: 1
				seconds: 1
				nanoSeconds: 1).
	self
		assert:
			aDuration
				-
					(Duration
						days: 0
						hours: 3
						minutes: 0
						seconds: 5
						nanoSeconds: 0)
		equals:
			(Duration
				days: 0
				hours: 23
				minutes: 2
				seconds: 59
				nanoSeconds: 5)
]

{ #category : #tests }
DurationTest >> testMinutes [
	self assert: aDuration minutes equals: 3.
	self assert: (Duration minutes: 3) minutes equals: 3
]

{ #category : #tests }
DurationTest >> testModulo [
	| d1 d2 d3 |
	d1 := 11.5 seconds.
	d2 := d1 \\ 3.
	self assert: d2 equals: (Duration nanoSeconds: 1).

	d3 := d1 \\ 3 seconds.
	self assert: d3 equals: (Duration seconds: 2 nanoSeconds: 500000000).

	self
		assert: aDuration \\ aDuration
		equals:
			(Duration
				days: 0
				hours: 0
				minutes: 0
				seconds: 0
				nanoSeconds: 0).
	self
		assert: aDuration \\ 2
		equals:
			(Duration
				days: 0
				hours: 0
				minutes: 0
				seconds: 0
				nanoSeconds: 1)
]

{ #category : #tests }
DurationTest >> testMultiply [
	self
		assert: aDuration * 2
		equals:
			(Duration
				days: 2
				hours: 4
				minutes: 6
				seconds: 8
				nanoSeconds: 10)
]

{ #category : #tests }
DurationTest >> testNanoSecond [
	self
		assert: (Duration nanoSeconds: 5) equals: 5 nanoSecond;
		assert: 0.5 nanoSecond equals: (Duration nanoSeconds: 0.5);
		assert: (1 / 2) nanoSecond equals: (Duration nanoSeconds: 0.5)
]

{ #category : #tests }
DurationTest >> testNanoSeconds [
	self assert: aDuration nanoSeconds equals: 5.
	self assert: (Duration nanoSeconds: 5) nanoSeconds equals: 5
]

{ #category : #tests }
DurationTest >> testNegated [
	self assert: aDuration + aDuration negated equals: (Duration seconds: 0)
]

{ #category : #tests }
DurationTest >> testNegative [
	self deny: aDuration negative.
	self assert: aDuration negated negative

]

{ #category : #tests }
DurationTest >> testNew [
	"self assert: Duration new =  (Duration seconds: 0)."
    "new is not valid as a creation method: MessageNotUnderstood: UndefinedObject>>quo:, where Duration seconds is nil"
]

{ #category : #tests }
DurationTest >> testNormalizeNanoSeconds [
	"Subtraction of two DateAndTime values may result in a request to
	create a Duration with negative nanoseconds and positive seconds.
	The resulting Duration should be normalized, otherwise its printString
	will be invalid."

	| d t1 t2 |
	t1 := '2004-01-07T11:55:01+00:00' asDateAndTime.
	t2 := '2004-01-07T11:55:00.9+00:00' asDateAndTime.
	d := t1 - t2.	"100 millisecond difference"
	self assert: d nanoSeconds > 0.
	self assert: d seconds equals: 0.
	self assert: d nanoSeconds equals: 100000000.
	self assert: d asString equals: '0:00:00:00.1'.
	"Verify that other combinations produces reasonable printString values"
	self assert: (Duration seconds: 1 nanoSeconds: 100000000) printString equals: '0:00:00:01.1'.
	self assert: (Duration seconds: -1 nanoSeconds: -100000000) printString equals: '-0:00:00:01.1'.
	self assert: (Duration seconds: 1 nanoSeconds: -100000000) printString equals: '0:00:00:00.9'.
	self assert: (Duration seconds: -1 nanoSeconds: 100000000) printString equals: '-0:00:00:00.9'
]

{ #category : #tests }
DurationTest >> testNumberConvenienceMethods [
	self
		assert: 1 week equals: (Duration days: 7);
		assert: -1 week equals: (Duration days: -7);
		assert: 1 day equals: (Duration days: 1);
		assert: -1 day equals: (Duration days: -1);
		assert: 1 hours equals: (Duration hours: 1);
		assert: -1 hour equals: (Duration hours: -1);
		assert: 1 minute equals: (Duration seconds: 60);
		assert: -1 minute equals: (Duration seconds: -60);
		assert: 1 second equals: (Duration seconds: 1);
		assert: -1 second equals: (Duration seconds: -1);
		assert: 1 milliSecond equals: (Duration milliSeconds: 1);
		assert: -1 milliSecond equals: (Duration milliSeconds: -1);
		assert: 1 nanoSecond equals: (Duration nanoSeconds: 1);
		assert: -1 nanoSecond equals: (Duration nanoSeconds: -1)
]

{ #category : #tests }
DurationTest >> testPlus [
	self assert: aDuration + 0 hours equals: aDuration.
	self
		assert: aDuration + aDuration
		equals:
			(Duration
				days: 2
				hours: 4
				minutes: 6
				seconds: 8
				nanoSeconds: 10)
]

{ #category : #tests }
DurationTest >> testPositive [
	self assert: (Duration nanoSeconds: 0) positive.
	self assert: aDuration positive.
	self deny: aDuration negated positive

]

{ #category : #tests }
DurationTest >> testPrintHumanReadableOn [
	self testHumanReadablePrintString
]

{ #category : #tests }
DurationTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDuration printOn: str ]) equals: '1:02:03:04.000000005'
]

{ #category : #tests }
DurationTest >> testQuotient [
	| d1 d2 q |
	d1 := 11.5 seconds.
	d2 := d1 // 3.
	self assert: d2 equals: (Duration seconds: 3 nanoSeconds: 833333333).

	q := d1 // 3 seconds.
	self assert: q equals: 3
]

{ #category : #tests }
DurationTest >> testReadFrom [
	self assert: aDuration equals: (Duration readFrom: '1:02:03:04.000000005' readStream)
]

{ #category : #tests }
DurationTest >> testReadFromBogus [

	self should: [Duration readFrom: '+0:01:02' readStream] raise: Error.
	"Seconds should be supplied as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"
]

{ #category : #tests }
DurationTest >> testReadFromMillisecond [
	self assert: (Duration readFrom: '0:00:00:00.001 ' readStream) nanoSeconds equals: 1000000
]

{ #category : #tests }
DurationTest >> testReadFromNoException [	

	#(	'0:00:00:00' 
		'0:00:00:00.000000001' 
		'0:00:00:00.999999999' 
		'0:00:00:00.100000000' 
		'0:00:00:00.10' 
		'0:00:00:00.1' 
		'0:00:00:01' 
		'0:12:45:45' 
		'1:00:00:00' 
		'365:00:00:00' 
		'-7:09:12:06.10' 
		'+0:01:02:55' 
		'+0:01:02:3') do: [:each | each asDuration printString = each]
]

{ #category : #tests }
DurationTest >> testRoundTo [
	self assert: (5 minutes + 37 seconds roundTo: 2 minutes) equals: 6 minutes.

	self
		assert: (aDuration roundTo: (Duration days: 1))
		equals:
			(Duration
				days: 1
				hours: 0
				minutes: 0
				seconds: 0
				nanoSeconds: 0).
	self
		assert: (aDuration roundTo: (Duration hours: 1))
		equals:
			(Duration
				days: 1
				hours: 2
				minutes: 0
				seconds: 0
				nanoSeconds: 0).
	self
		assert: (aDuration roundTo: (Duration minutes: 1))
		equals:
			(Duration
				days: 1
				hours: 2
				minutes: 3
				seconds: 0
				nanoSeconds: 0)
]

{ #category : #tests }
DurationTest >> testSeconds [
	self assert: aDuration seconds equals: 4.
	self assert: (Duration nanoSeconds: 2) seconds equals: 0.
	self assert: (Duration seconds: 2) seconds equals: 2.
	self
		assert:
			(Duration
				days: 1
				hours: 2
				minutes: 3
				seconds: 4) seconds
		equals: 4.
	self
		deny:
			(Duration
				days: 1
				hours: 2
				minutes: 3
				seconds: 4) seconds
		equals: 1 * 24 * 60 * 60 + (2 * 60 * 60) + (3 * 60) + 4
]

{ #category : #tests }
DurationTest >> testSecondsNanoSeconds [
	self assert: (Duration seconds: 0 nanoSeconds: 5) equals: (Duration nanoSeconds: 5).
	"not sure I should include in sunit since its Private "
	self assert: (aDuration seconds: 0 nanoSeconds: 1) equals: (Duration nanoSeconds: 1)
]

{ #category : #tests }
DurationTest >> testStoreOn [
	| stream |
	aDuration storeOn: (stream := (String new: 20) writeStream).
	self assert: stream contents equals: '(Duration seconds: 93784 nanoSeconds: 5)'
]

{ #category : #tests }
DurationTest >> testTicks [
	self assert: aDuration ticks equals: #(1 7384 5)
]

{ #category : #tests }
DurationTest >> testTotalSeconds [
	self assert: 0 seconds totalSeconds equals: 0.
	self assert: 1 second totalSeconds equals: 1.
	self assert: 100 second totalSeconds equals: 100.
	self assert: 500 milliSeconds totalSeconds equals: 1/2.
	self assert: 1500 milliSeconds totalSeconds equals: 3/2.
	self assert: 2500 milliSeconds totalSeconds equals: 5/2.
	self assert: 1 second negated totalSeconds equals: -1
]

{ #category : #tests }
DurationTest >> testTruncateTo [
	self assert: (5 minutes + 37 seconds truncateTo: 2 minutes) equals: 4 minutes.
	self
		assert: (aDuration truncateTo: (Duration days: 1))
		equals:
			(Duration
				days: 1
				hours: 0
				minutes: 0
				seconds: 0
				nanoSeconds: 0).
	self
		assert: (aDuration truncateTo: (Duration hours: 1))
		equals:
			(Duration
				days: 1
				hours: 2
				minutes: 0
				seconds: 0
				nanoSeconds: 0).
	self
		assert: (aDuration truncateTo: (Duration minutes: 1))
		equals:
			(Duration
				days: 1
				hours: 2
				minutes: 3
				seconds: 0
				nanoSeconds: 0)
]

{ #category : #tests }
DurationTest >> testWeeks [
	self assert: (Duration weeks: 1) days equals: 7
]

{ #category : #tests }
DurationTest >> testWholeMicroseconds [
	self assert: 0 seconds wholeMicroseconds equals: 0.
	self assert: 1 second wholeMicroseconds equals: 0.
	self assert: 1002003004 nanoSeconds wholeMicroseconds equals: 3
]

{ #category : #tests }
DurationTest >> testWholeMilliseconds [
	self assert: 0 seconds wholeMilliseconds equals: 0.
	self assert: 1 second wholeMilliseconds equals: 0.
	self assert: 1002003004 nanoSeconds wholeMilliseconds equals: 2
]

{ #category : #tests }
DurationTest >> testWholeNanoseconds [
	self assert: 0 seconds wholeNanoseconds equals: 0.
	self assert: 1 second wholeNanoseconds equals: 0.
	self assert: 1002003004 nanoSeconds wholeNanoseconds equals: 4
]

{ #category : #tests }
DurationTest >> testZero [
	self assert: Duration zero equals: (Duration seconds: 0)
]
