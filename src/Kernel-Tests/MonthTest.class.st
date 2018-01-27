"
This is the unit test for the class Month.

"
Class {
	#name : #MonthTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'month'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
MonthTest >> classToBeTested [

	^ Month
]

{ #category : #coverage }
MonthTest >> selectorsToBeIgnored [

	| deprecated private special |
	deprecated := #().
	private := #( #printOn: ).
	special := #( #next ).

	^ super selectorsToBeIgnored, deprecated, private, special.
]

{ #category : #running }
MonthTest >> setUp [

	super setUp.
	month := Month year: 1998 month: 7.
]

{ #category : #running }
MonthTest >> tearDown [

	month := nil.
	super tearDown.	
]

{ #category : #tests }
MonthTest >> testConverting [

	self assert: month asDate = '1 July 1998' asDate
]

{ #category : #tests }
MonthTest >> testEnumerating [
	| weeks |
	weeks := OrderedCollection new.
	month weeksDo: [ :w | weeks add: w start ].
	0 to: 4 do: [ :i | weeks remove: (Week starting:  ('29 June 1998' asDate addDays: i * 7)) start ].
	self assert: weeks isEmpty
]

{ #category : #tests }
MonthTest >> testIndexOfMonth [	
       | m |
       m := #(#January #February #March #April #May #June #July #August
#September #October #November #December).

       m withIndexDo: [:item :index | self assert: (Month indexOfMonth:
item) = index].

       self should: [Month indexOfMonth: 1] raise: self defaultTestError.
       self should: [Month indexOfMonth: #Marsh] raise: self defaultTestError.
"notice the misspell!!"
       self should: [Month indexOfMonth: #UnexistingMonth] raise:
 self defaultTestError.
]

{ #category : #tests }
MonthTest >> testInquiries [

	self 
		assert: month index = 7;
		assert: month name = #July;
		assert: month duration = (31 days).

]

{ #category : #tests }
MonthTest >> testInstanceCreation [
	| m1 m2 |
	m1 := Month starting:  '4 July 1998' asDate.
	m2 := Month year: 1998 month: #July .
	self
		assert: month = m1;
		assert: month = m2
]

{ #category : #tests }
MonthTest >> testNameOfMonth [

       | m |
       m := #(#January #February #March #April #May #June #July #August
#September #October #November #December).

       m withIndexDo: [:item :index | self assert: (Month nameOfMonth:
index) = item].

       self should: [Month nameOfMonth: 0] raise: self defaultTestError.
       self should: [Month nameOfMonth: 13] raise: self defaultTestError.
       self should: [Month nameOfMonth: #January] raise: self defaultTestError.
]

{ #category : #tests }
MonthTest >> testOffset [
	"Check that the offset is maintained when creating a new instance of Month from a DateAndTime"

	| dt newMonth |

	dt := DateAndTime fromString: '2018/01/01T00:00:00+10'.
	newMonth := Month starting: dt duration: 0. "duration is ignored"
	self assert: newMonth asDateAndTime offset equals: dt offset
]

{ #category : #tests }
MonthTest >> testPreviousNext [
	| n p |
	n := month next.
	p := month previous.

	self
		assert: n year = 1998;
		assert: n index = 8;
		assert: p year = 1998;
		assert: p index = 6.


]

{ #category : #tests }
MonthTest >> testPrinting [

	self 
		assert: month printString = 'July 1998'.

]

{ #category : #tests }
MonthTest >> testReadFrom [

	| m |
	m := Month readFrom: 'July 1998' readStream.
	self 
		assert: m = month
]
