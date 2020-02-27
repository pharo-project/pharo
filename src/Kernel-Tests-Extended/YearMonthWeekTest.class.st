"
I am one of several Sunit test Cases intentended to provide complete coverage for the Chronology set of classes as part of the external testing. See DateAndEpochTestCase for a complete list.  tlk.
I have no fixtures but do make sure to restore anything I change.
"
Class {
	#name : #YearMonthWeekTest,
	#superclass : #TestCase,
	#instVars : [
		'restoredStartDay',
		'restoredTimeZone'
	],
	#category : #'Kernel-Tests-Extended-Chronology'
}

{ #category : #running }
YearMonthWeekTest >> setUp [
	super setUp.
	restoredStartDay := Week startDay.
	restoredTimeZone := DateAndTime localTimeZone.

	Week startDay: #Sunday.
	DateAndTime localTimeZone: (TimeZone timeZones detect: [:tz | tz abbreviation = 'GMT']).
]

{ #category : #running }
YearMonthWeekTest >> tearDown [
	Week startDay: restoredStartDay.
	DateAndTime localTimeZone: restoredTimeZone.
	super tearDown 
]

{ #category : #tests }
YearMonthWeekTest >> testDaysInMonth [
	self assert: (Month daysInMonth: 2 forYear: 2000) equals: 29.
	self assert: (Month daysInMonth: 2 forYear: 2001) equals: 28.
	self assert: (Month daysInMonth: 2 forYear: 2004) equals: 29.
	self assert: (Month daysInMonth: 2 forYear: 2100) equals: 28.

	self assert: (Month daysInMonth: 'January' forYear: 2003) equals: 31.
	self assert: (Month daysInMonth: 'February' forYear: 2003) equals: 28.
	self assert: (Month daysInMonth: 'March' forYear: 2003) equals: 31.
	self assert: (Month daysInMonth: 'April' forYear: 2003) equals: 30.
	self assert: (Month daysInMonth: 'May' forYear: 2003) equals: 31.
	self assert: (Month daysInMonth: 'June' forYear: 2003) equals: 30.
	self assert: (Month daysInMonth: 'July' forYear: 2003) equals: 31.
	self assert: (Month daysInMonth: 'August' forYear: 2003) equals: 31.
	self assert: (Month daysInMonth: 'September' forYear: 2003) equals: 30.
	self assert: (Month daysInMonth: 'October' forYear: 2003) equals: 31.
	self assert: (Month daysInMonth: 'November' forYear: 2003) equals: 30.
	self assert: (Month daysInMonth: 'December' forYear: 2003) equals: 31
]

{ #category : #tests }
YearMonthWeekTest >> testDaysInYear [
	self assert: (Year daysInYear: 2000) equals: 366.
	self assert: (Year daysInYear: 2001) equals: 365.
	self assert: (Year daysInYear: 2004) equals: 366.
	self assert: (Year daysInYear: 2100) equals: 365.
	self assert: (Year daysInYear: 2003) equals: 365
]

{ #category : #tests }
YearMonthWeekTest >> testIndexOfDay [
	self assert: (Week indexOfDay: 'Friday') equals: 6
]

{ #category : #tests }
YearMonthWeekTest >> testIsLeapYear [
	self assert: (Year isLeapYear: 2000).
	self deny: (Year isLeapYear: 2001).
	self assert: (Year isLeapYear: 2004).
	self deny: (Year isLeapYear: 2100).
	self deny: (Year isLeapYear: 2002).
]

{ #category : #tests }
YearMonthWeekTest >> testMonthPrintOn [
	| aMonth |
	aMonth := Month starting: DateAndTime new duration: 31 days.
	self assert: (String streamContents: [ :str | aMonth printOn: str ]) equals: 'January 1901'
]

{ #category : #tests }
YearMonthWeekTest >> testStartDay [
	Week startDay: 'Wednesday'.
	self assert: Week startDay equals: 'Wednesday'.
	Week startDay: 'Thursday'.
	self assert: Week startDay equals: 'Thursday'
]

{ #category : #tests }
YearMonthWeekTest >> testWeekPrintOn [
	| aWeek cs rw |
	aWeek := Week starting: (DateAndTime year: 1900 month: 12 day: 31).
	cs := 'a Week starting: 1900-12-30T00:00:00+00:00'.
	rw := String new writeStream.
	aWeek printOn: rw.
	self assert: rw contents equals: cs
]

{ #category : #tests }
YearMonthWeekTest >> testYearPrintOn [
	| aYear |
	aYear := Year starting: DateAndTime new duration: 365 days.
	self assert: (String streamContents: [ :str | aYear printOn: str ]) equals: 'a Year (1901)'
]
