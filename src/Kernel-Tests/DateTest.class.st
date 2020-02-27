"
SUnit tests for class Date
"
Class {
	#name : #DateTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'aTime',
		'january23rd2004',
		'june2nd1973'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
DateTest >> classToBeTested [

	^ self dateClass
]

{ #category : #private }
DateTest >> dateClass [

	^ Date
]

{ #category : #helpers }
DateTest >> epoch [

	^ Date year: 1901 month: 1 day: 1
]

{ #category : #helpers }
DateTest >> restoreLocalTimeZoneAfter: aBlock [

	| realTimeZone |
	realTimeZone := DateAndTime localTimeZone.
	aBlock ensure: [ DateAndTime localTimeZone: realTimeZone ].
]

{ #category : #coverage }
DateTest >> selectorsToBeIgnored [

	 | deprecated private special |
	deprecated := #().
	private := #().
	special := #( #< #= #new #next #previous #printOn: #printOn:format: #storeOn: #fromString: ).

	^ super selectorsToBeIgnored, deprecated, private, special
]

{ #category : #running }
DateTest >> setUp [
	super setUp.
	june2nd1973 := self dateClass year: 1973 day: 153.
	january23rd2004 := Date readFrom: '01-23-2004' readStream.
	aTime := Time readFrom: '12:34:56 pm' readStream
]

{ #category : #tests }
DateTest >> testAccessing [

	self	
		assert: june2nd1973 day equals: 153;
		assert: june2nd1973 julianDayNumber equals: 2441836;
		assert: june2nd1973 monthIndex equals: 6;
		assert: june2nd1973 monthName equals: #June;
		assert: june2nd1973 weekday equals: #Saturday;
		assert: june2nd1973 weekdayIndex equals: 7;
		assert: june2nd1973 year equals: 1973.

]

{ #category : #tests }
DateTest >> testAddDays [

	self assert: (january23rd2004 addDays: 0) equals: '2004-01-23' asDate.	
	self assert: (january23rd2004 addDays: 31) equals: '2004-02-23' asDate.
	self assert: (january23rd2004 addDays: 366) equals: '2005-01-23' asDate.
]

{ #category : #tests }
DateTest >> testAddMonths [

	self assert: (january23rd2004 addMonths: 0) equals: '2004-01-23' asDate.
	self assert: (january23rd2004 addMonths: 1) equals: '2004-02-23' asDate.
	self assert: (january23rd2004 addMonths: 12) equals: '2005-01-23' asDate.
]

{ #category : #tests }
DateTest >> testArithmetic [

	| laterDate |
	laterDate := june2nd1973 addDays: 32.

	self 
		assert: (laterDate subtractDate: june2nd1973) equals: 32;
		assert: (june2nd1973 subtractDate: laterDate) equals: -32;
		assert: (laterDate subtractDays: 32) equals: june2nd1973.

]

{ #category : #tests }
DateTest >> testAsDate [

	self assert: january23rd2004 asDate equals: january23rd2004.
]

{ #category : #tests }
DateTest >> testAsMonthInYear [
	
	self assert: ((Date asMonth: 9 inYear: 2018) asString) equals: 'September 2018'.
]

{ #category : #tests }
DateTest >> testAsSeconds [

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
]

{ #category : #tests }
DateTest >> testBasicPrinting [
	| printer |
	printer := BasicDatePrinter new.
	self
		assert: (String streamContents: [ :s | printer printDate: january23rd2004 format: #() on: s ])
		equals: '2004-01-23'.
	self
		assert: (String streamContents: [ :s | printer printDate: june2nd1973 format: #() on: s ])
		equals: '1973-06-02'
]

{ #category : #tests }
DateTest >> testComparing [

	| sameDate laterDate earlierDate |
	sameDate := june2nd1973 asString asDate.
	laterDate := june2nd1973 + 1 day.
	earlierDate := june2nd1973 - 1 day.

	self
		assert: june2nd1973 equals: sameDate;
		assert: june2nd1973 equals: june2nd1973 copy;
		assert: june2nd1973 hash equals: sameDate hash.
	self 
		assert: june2nd1973 < laterDate;
		assert: june2nd1973 > earlierDate.

]

{ #category : #tests }
DateTest >> testDayMonthYearDo [
	
	january23rd2004 dayMonthYearDo: [ :day :month :year | 
		self assert: day equals: 23.
		self assert: month equals: 1.
		self assert: year equals: 2004 ].
]

{ #category : #tests }
DateTest >> testDayOfWeek [

	| dayNames sundayIndex |
	
	sundayIndex := self dateClass dayOfWeek: #Sunday.
	self assert: sundayIndex equals: 1.
	
	dayNames := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	dayNames doWithIndex: [ :e :i | | dayIndex |
		dayIndex := self dateClass dayOfWeek: e.
		self assert: dayIndex equals: i ].
]

{ #category : #tests }
DateTest >> testDaysInMonthForYear [

	self assert: (Date daysInMonth: #February forYear: 2008) equals: 29.	
	self assert: (Date daysInMonth: #February forYear: 2000) equals: 29.	
	self assert: (Date daysInMonth: #February forYear: 2100) equals: 28.	
	self assert: (Date daysInMonth: #July forYear: 2100) equals: 31.	
]

{ #category : #tests }
DateTest >> testDaysInYear [

	self assert: (Date daysInYear: 2000) equals: 366.	
	self assert: (Date daysInYear: 2008) equals: 366.	
	self assert: (Date daysInYear: 2100) equals: 365	

]

{ #category : #tests }
DateTest >> testDuration [

	self assert: january23rd2004 duration equals: 24 hours.
]

{ #category : #tests }
DateTest >> testEqual [
	self assert: january23rd2004 equals: 'January 23, 2004' asDate
]

{ #category : #tests }
DateTest >> testEquals [

	| tzm8 tzp10 |
	
	tzm8 := january23rd2004 translateTo: -8 hours.
	tzp10 := january23rd2004 translateTo: 10 hours.

	self
		assert: (january23rd2004 equals: tzm8);
		assert: (january23rd2004 equals: tzp10);
		assert: (tzm8 equals: tzp10);
		assert: (january23rd2004 equals: june2nd1973) not.
]

{ #category : #tests }
DateTest >> testFirstWeekdayOfMonthYear [

	self assert: (Date firstWeekdayOfMonth: 'January' year: 2004) equals: 5.	

]

{ #category : #tests }
DateTest >> testFromDays [

	| march18th1627 epochFromDays june2nd1973FromDays march18th1627FromDays january23rd2004FromDays |

	epochFromDays := self dateClass fromDays: 0.
	self assert: epochFromDays equals: (self epoch translateTo: 0).

	june2nd1973FromDays := self dateClass fromDays: (june2nd1973 - self epoch) asDays.
	self assert: june2nd1973FromDays equals: (june2nd1973 translateTo: 0).

	march18th1627 := '18 March 1627' asDate.
	march18th1627FromDays := self dateClass fromDays: (march18th1627 - self epoch) asDays.
	self assert: march18th1627FromDays equals: (march18th1627 translateTo: 0).
	
	january23rd2004FromDays := self dateClass fromDays: 103*365 "years" + 22 "days since Jan 1" + 25 "leap days".
	self assert: january23rd2004FromDays equals: (january23rd2004 translateTo: 0).	
]

{ #category : #tests }
DateTest >> testFromSeconds [

	| d |
	d := self dateClass fromSeconds: june2nd1973 asSeconds. 
	self assert: d equals: june2nd1973.

]

{ #category : #tests }
DateTest >> testFromString [
 	"The fromString method requires month, day and year delemited by a valid date delimiter"
	
	| d |
	d := self dateClass fromString: '06.02.1973'. 
	self assert: d equals: june2nd1973.
 
	d := self dateClass fromString: '06-02-1973'. 
	self assert: d equals: june2nd1973.

	d := self dateClass fromString: '06/02/1973'. 
	self assert: d equals: june2nd1973.	
]

{ #category : #tests }
DateTest >> testIndexOfMonth [

	self assert: (Date indexOfMonth: #January) equals: 1.	
	self assert: (Date indexOfMonth: #December) equals: 12.
	
	self should: [ Date indexOfMonth: #NonExistantMonth ] raise: Error.
]

{ #category : #tests }
DateTest >> testInquiries [

	self	
		assert: june2nd1973 dayOfMonth equals: 2;
		assert: june2nd1973 dayOfYear equals: 153;
		assert: june2nd1973 daysInMonth equals: 30;
		assert: june2nd1973 daysInYear equals: 365;
		assert: june2nd1973 daysLeftInYear equals: (365 - 153);
		assert: june2nd1973 firstDayOfMonth equals: 152.

]

{ #category : #tests }
DateTest >> testIsAfter [

	| tzm8 tzp10 |
	
	tzm8 := january23rd2004 translateTo: -8 hours.
	tzp10 := january23rd2004 translateTo: 10 hours.

	self
		assert: (tzp10 isAfter: tzm8) not;
		assert: (january23rd2004 isAfter: june2nd1973);
		assert: (june2nd1973 isAfter: june2nd1973) not.
]

{ #category : #tests }
DateTest >> testIsBefore [

	| tzm8 tzp10 |
	
	tzm8 := january23rd2004 translateTo: -8 hours.
	tzp10 := january23rd2004 translateTo: 10 hours.

	self
		assert: (tzm8 isBefore: tzp10) not;
		assert: (june2nd1973 isBefore: january23rd2004);
		assert: (june2nd1973 isBefore: june2nd1973) not.
]

{ #category : #tests }
DateTest >> testIsLeapYear [ 

	| dateInLeapYear dateInNonLeapYear |
	dateInLeapYear := january23rd2004.
	dateInNonLeapYear := dateInLeapYear + 365 days.
	self assert: dateInLeapYear isLeapYear equals: true.
	self assert: dateInNonLeapYear isLeapYear equals: false.



]

{ #category : #tests }
DateTest >> testIsOnOrAfter [

	| tzm8 tzp10 |
	
	tzm8 := january23rd2004 translateTo: -8 hours.
	tzp10 := january23rd2004 translateTo: 10 hours.

	self
		assert: (tzp10 isOnOrAfter: tzm8);
		assert: (june2nd1973 isOnOrAfter: january23rd2004) not;
		assert: (june2nd1973 isOnOrAfter: june2nd1973);
		assert: (january23rd2004 isOnOrAfter: june2nd1973).
]

{ #category : #tests }
DateTest >> testIsOnOrBefore [

	| tzm8 tzp10 |
	
	tzm8 := january23rd2004 translateTo: -8.
	tzp10 := january23rd2004 translateTo: 10.

	self
		assert: (tzm8 isOnOrBefore: tzp10);
		assert: (june2nd1973 isOnOrBefore: january23rd2004);
		assert: (june2nd1973 isOnOrBefore: june2nd1973);
		assert: (january23rd2004 isOnOrBefore: june2nd1973) not.
]

{ #category : #tests }
DateTest >> testJulianDayNumber [

	self assert: (january23rd2004 translateTo: 0) equals: (Date julianDayNumber: ((4713+2004)*365 +1323) offset: 0 hour).
]

{ #category : #tests }
DateTest >> testMmddyyyy [
	self assert: january23rd2004 mmddyyyy equals: '1/23/2004'
]

{ #category : #tests }
DateTest >> testNameOfDay [

	| dayNames firstDayName |
	
	firstDayName := self dateClass nameOfDay: 1.
	self assert: firstDayName equals: #Sunday.
	
	dayNames := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	(1 to: 7) do: [ :i | | dayName |
		dayName := self dateClass nameOfDay: i.
		self assert: dayName equals: (dayNames at: i) ].
]

{ #category : #tests }
DateTest >> testNameOfMonth [

	self assert: (Date nameOfMonth: 1) equals: #January.	
	self assert: (Date nameOfMonth: 12) equals: #December.

	self should: [ Date nameOfMonth: 0 ] raise: SubscriptOutOfBounds.
	self should: [ Date nameOfMonth: 13 ] raise: SubscriptOutOfBounds.

]

{ #category : #tests }
DateTest >> testNew [
	| instance epoch |
	instance := self dateClass new.
	epoch := self epoch.
	"We have to be careful, since #new creates a Date for midnight in the local timezone, whereas #epoch is based on a fixed time in UTC"
	instance offset negative 
		ifTrue: [ epoch := epoch - 1 day ]. 
	self
		assert: instance
		equals: epoch.
]

{ #category : #tests }
DateTest >> testNewDayMonthYear [

	self assert: (Date year: 2004 month: 1 day: 23) equals: january23rd2004.	

]

{ #category : #tests }
DateTest >> testNewDayYear [

	self assert: (Date year: 2004 day: 23) equals: january23rd2004	

]

{ #category : #tests }
DateTest >> testNext [

	| nextDay |
	nextDay := june2nd1973 next.
	self assert: nextDay equals: '3 June, 1973' asDate.
]

{ #category : #tests }
DateTest >> testPrevious [

	| previousDay |
	previousDay := june2nd1973 previous.
	self assert: previousDay equals: '1 June, 1973' asDate.
]

{ #category : #tests }
DateTest >> testPreviousByName [

	self assert: (january23rd2004 previous: #Friday) equals: '2004-01-16' asDate.


]

{ #category : #tests }
DateTest >> testPrintFormat [
	| printFormat printedDate |
	printFormat := #(1 2 3 $/ 1 1).
	self assert: (january23rd2004 printFormat: printFormat) equals: '23/1/2004'.

	printFormat at: 4 put: $-.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: printedDate equals: '23-1-2004'.

	"Month type 1 should print the index of the month."
	printFormat at: 5 put: 1.
	self assert: ((printedDate splitOn: $-) at: 2) equals: '1'.

	"Month type 2 should print the abbreviation of the month name."
	printFormat at: 5 put: 2.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 2) equals: 'Jan'.

	"Month type 2 should print the full month name."
	printFormat at: 5 put: 3.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 2) equals: 'January'.

	"Month type 4 should not print the month."
	printFormat at: 5 put: 4.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 2) equals: ''.

	"Year type 1 should print the full year."
	printFormat at: 6 put: 1.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 3) equals: '2004'.

	"Year type 2 should print the year abbreviation."
	printFormat at: 6 put: 2.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 3) equals: '04'.

	"Year type 2 should print the year abbreviation."
	printFormat at: 6 put: 2.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 3) equals: '04'.

	"Out of range year types -> short year."
	printFormat at: 6 put: 3.
	printedDate := january23rd2004 printFormat: printFormat.
	self assert: ((printedDate splitOn: $-) at: 3) equals: '04'.

	"Usage examples"
	self
		assert: june2nd1973 mmddyyyy equals: '6/2/1973';
		assert: june2nd1973 yyyymmdd equals: '1973-06-02';
		assert: (june2nd1973 printFormat: #(3 1 2 $! 2 1 1)) equals: '1973!2!Jun';
		assert: (june2nd1973 printFormat: #(1 2 3 0 1 1 2)) equals: '02061973';
		assert: (june2nd1973 printFormat: #(2 1 3 0 1 1 2)) equals: '06021973';
		assert: (june2nd1973 printFormat: #(3 2 1 0 1 1 2)) equals: '19730602';
		assert: (june2nd1973 printFormat: #(1 2 3 0 1 1 1)) equals: '261973'
]

{ #category : #tests }
DateTest >> testPrintOn [
	self assert: (String streamContents: [ :str | january23rd2004 printOn: str ]) equals: '23 January 2004'
]

{ #category : #tests }
DateTest >> testPrintOnFormat [
	self assert: (String streamContents: [ :str | january23rd2004 printOn: str format: #(3 2 1 $* 2 2) ]) equals: '04*Jan*23'
]

{ #category : #tests }
DateTest >> testReadFrom [

	| s1 s2 s3 s4 s5 | 
	s1 := '2 June 1973' readStream.
	s2 := '2-JUN-73' readStream.
	s3 := 'June 2, 1973' readStream.
	s4 := '6/2/73' readStream.
	s5 := '2JUN73' readStream.

	self	
		assert: (self dateClass readFrom: s1) equals: june2nd1973;
		assert: (self dateClass readFrom: s2) equals: june2nd1973;
		assert: (self dateClass readFrom: s3) equals: june2nd1973;
		assert: (self dateClass readFrom: s4) equals: june2nd1973.
]

{ #category : #tests }
DateTest >> testStarting [

	| aDateAndTime anyTime |
	anyTime := '13:12' asTime.
	aDateAndTime := DateAndTime date: january23rd2004 time: anyTime.
	self assert: (Date starting: aDateAndTime) equals: january23rd2004.  
]

{ #category : #tests }
DateTest >> testStoreOn [
	self assert: (String streamContents: [ :str | january23rd2004 storeOn: str ]) equals: '''23 January 2004'' asDate'
]

{ #category : #tests }
DateTest >> testSubtractDate [

	self assert: (january23rd2004 subtractDate: january23rd2004 previous) equals: 1.	
	self assert: (january23rd2004 subtractDate: january23rd2004) equals: 0.	
	self assert: (january23rd2004 subtractDate: january23rd2004 next) equals: -1.	

]

{ #category : #tests }
DateTest >> testSubtractDays [

	self assert: (january23rd2004 subtractDays: 0) equals: '2004-01-23' asDate.	
	self assert: (january23rd2004 subtractDays: 30) equals: '2003-12-24' asDate.
]

{ #category : #tests }
DateTest >> testTomorrow [

	"Not a great test: could falsely fail if midnight come in between the two executions"
	self assert: Date tomorrow equals: Date today + 1 day.
]

{ #category : #tests }
DateTest >> testWeekday [

	self assert: january23rd2004 weekday equals: #Friday.	
	self assert: january23rd2004 weekdayIndex equals: 6.
]

{ #category : #tests }
DateTest >> testYesterday [

	"Not a great test: could falsely fail if midnight come in between the two executions"
	self assert: Date yesterday equals: Date today - 1 day.

]

{ #category : #tests }
DateTest >> testYyyymmdd [

	self assert: january23rd2004 yyyymmdd equals: '2004-01-23'
]

{ #category : #helpers }
DateTest >> useNonUtcTimeZoneDuring: aBlock [

	self useTimeZone: 'EDT' during: aBlock.
]

{ #category : #helpers }
DateTest >> useTimeZone: abbreviation during: aBlock [

  | timeZone |
  timeZone := TimeZone abbreviated: abbreviation.
  self restoreLocalTimeZoneAfter: [ 
    DateAndTime localTimeZone: timeZone. 
    aBlock cull: timeZone ].
]
