"
SUnit tests for year handling
"
Class {
	#name : #YearTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
YearTest >> classToBeTested [

	^ Year
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
	self assert: (Year year: (leap year - 1)) = leap previous 
]

{ #category : #tests }
YearTest >> testStart [
		
	| yyyy |
	yyyy := DateAndTime now year.
	self assert: Year current start = (DateAndTime year: yyyy month: 1 day: 1)
]
