"
SUnit tests for year handling
"
Class {
	#name : #YearTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Extended-Chronology'
}

{ #category : #coverage }
YearTest >> classToBeTested [

	^ Year
]

{ #category : #tests }
YearTest >> testFirstThursday [
		
	1800 to: 2000 do:
		[ :y |
			| firstThursday |
			firstThursday := (Year year: y) firstThursday.
			self assert: firstThursday dayOfWeek equals: 5.
			self assert: firstThursday year equals: y.
			self assert: (firstThursday - 1 week) year equals: y - 1 ]

]

{ #category : #tests }
YearTest >> testOffset [
	"Check that the offset is maintained when creating a new instance of Month from a DateAndTime"

	| dt newYear |

	dt := DateAndTime fromString: '2018/01/01T00:00:00+10'.
	newYear := Year starting: dt duration: 0. "duration is ignored"
	self assert: newYear asDateAndTime offset equals: dt offset
]

{ #category : #tests }
YearTest >> testPreviousInLeapYear [
	| leap |
	leap := Year year: 2008.
	self assert: leap isLeapYear.
	self assert: (Year year: leap year - 1) equals: leap previous
]

{ #category : #tests }
YearTest >> testStart [
	| yyyy |
	yyyy := DateAndTime now year.
	self assert: Year current start equals: (DateAndTime year: yyyy month: 1 day: 1)
]

{ #category : #tests }
YearTest >> testcurrentYear [
		
	| yyyy |
	yyyy := Year currentYear. 
	self assert: Date today asYear equals: yyyy
]

{ #category : #tests }
YearTest >> testcurrentYearNumber [
		
	| yyyy |
	yyyy := Year currentYearNumber. 
	self assert: Date today year equals: yyyy
]

{ #category : #tests }
YearTest >> testdaysInMonth [
	
	self assert: ((Year year: 2018) daysInMonth:1) equals: 31. "January"
	self assert: ((Year year: 2018) daysInMonth:2) equals: 28. "February"
	self assert: ((Year year: 2016) daysInMonth:2) equals: 29. "February Leap Year"
	self assert: ((Year year: 2018) daysInMonth:3) equals: 31. "March"
	self assert: ((Year year: 2018) daysInMonth:4) equals: 30. "April"
	self assert: ((Year year: 2018) daysInMonth:5) equals: 31. "May"
	self assert: ((Year year: 2018) daysInMonth:6) equals: 30. "June"
	self assert: ((Year year: 2018) daysInMonth:7) equals: 31. "July"
	self assert: ((Year year: 2018) daysInMonth:8) equals: 31. "August"
	self assert: ((Year year: 2018) daysInMonth:9) equals: 30. "September"
	self assert: ((Year year: 2018) daysInMonth:10) equals: 31. "October"
	self assert: ((Year year: 2018) daysInMonth:11) equals: 30. "November"
	self assert: ((Year year: 2018) daysInMonth:12) equals: 31. "December"

]
