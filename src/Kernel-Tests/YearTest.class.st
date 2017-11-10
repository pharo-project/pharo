"
SUnit tests for year handling
"
Class {
	#name : #YearTest,
	#superclass : #ClassTestCase,
	#category : #Kernel-Tests-Chronology
}

{ #category : #coverage }
YearTest >> classToBeTested [

	^ Year
]

{ #category : #tests }
YearTest >> testPreviousInLeapYear [
	"self debug: #testPreviousInLeapYear"
	
	| leap |
	leap := Year year: 2008.
	self assert: leap isLeapYear. 
	self assert: (Year year: (leap year - 1)) = leap previous 
]

{ #category : #tests }
YearTest >> testStart [
	"self debug: #testStart"
	
	| yyyy |
	yyyy := DateAndTime now year.
	self assert: Year current start = (DateAndTime year: yyyy month: 1 day: 1)
]
