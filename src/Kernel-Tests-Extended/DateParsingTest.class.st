"
SUnit tests for date parsing
"
Class {
	#name : #DateParsingTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Extended-Chronology'
}

{ #category : #asserting }
DateParsingTest >> assertReading: aString as: aPattern equals: aDate [
	self assert: (self reading: aPattern pattern: aString) equals: aDate
]

{ #category : #asserting }
DateParsingTest >> assertReading: aString as: aPattern raise: anErrorClass [

	self
		should: [ self reading: aPattern pattern: aString ]
		raise: anErrorClass
	
]

{ #category : #accessing }
DateParsingTest >> dateParserClass [
	^ DateParser
]

{ #category : #asserting }
DateParsingTest >> reading: aPattern pattern: aString [
	^ (self dateParserClass readingFrom: aString readStream pattern: aPattern) parse
]

{ #category : #tests }
DateParsingTest >> testCanEscapeSpecialCharacters [
	"Checks that reserved pattern characters $d $m and $y can be escaped in patterns"
	self assertReading: '4d\2m\2345y' as: 'd\d\\m\m\\y\y' equals: (Date year: 2345 month: 2 day: 4)
		
	
]

{ #category : #tests }
DateParsingTest >> testDefaultParsingSucceeds [ 
	| date |
	date := Date year: 2013 month: 11 day: 29.
	self assertReading: '29.11.2013' as: 'dd.mm.yyyy' equals: date.
	self assertReading: '29.11.2013' as: 'd.m.y' equals: date.
	self assertReading: '29.11.13' as: 'd.m.y' equals: date
]

{ #category : #'tests - dd pattern' }
DateParsingTest >> testParsingDDPatternWithIncorrectDaysFails [ 
	self assertReading: '02.00.2013' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.13.2013' as: 'dd.mm.yyyy' raise: DateError.
	
]

{ #category : #'tests - dd pattern' }
DateParsingTest >> testParsingDDPatternWithSingleDigitFails [
	self assertReading: '4.02.2345' as: 'dd.mm.yyyy' raise: DateError
]

{ #category : #'tests - d pattern' }
DateParsingTest >> testParsingDPatternWithIncorrectDayFails [ 
	self assertReading: '0.11.2013' as: 'd.mm.yyyy' raise: DateError.
	self assertReading: '32.2013.29' as: 'd.mm.yyyy' raise: DateError.
]

{ #category : #'tests - d pattern' }
DateParsingTest >> testParsingDPatternWithSingleDigitSucceeds [
	self assertReading: '2.11.2013' as: 'd.mm.yyyy' equals: (Date year: 2013 month: 11 day: 2)
]

{ #category : #'tests - d pattern' }
DateParsingTest >> testParsingDPatternWithTwoDigitSucceeds [ 
	self assertReading: '29.11.2013' as: 'd.mm.yyyy' equals: (Date year: 2013 month: 11 day: 29).
]

{ #category : #'tests - mm pattern' }
DateParsingTest >> testParsingMMPatternWithIncorrectMonthFails [ 
	self assertReading: '02.00.2013' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.13.2013' as: 'dd.mm.yyyy' raise: DateError
]

{ #category : #'tests - mm pattern' }
DateParsingTest >> testParsingMMPatternWithSingleDigitFails [
	self assertReading: '04.2.2345' as: 'dd.mm.yyyy' raise: DateError
]

{ #category : #'tests - m pattern' }
DateParsingTest >> testParsingMPatternWithIncorrectMonthFails [ 
	self assertReading: '1.0.2013' as: 'dd.m.yyyy' raise: DateError.
	self assertReading: '22.13.2013' as: 'dd.m.yyyy' raise: DateError.
]

{ #category : #'tests - m pattern' }
DateParsingTest >> testParsingMPatternWithSingleDigitSucceeds [ 
	| date |
	date := Date year: 2013 month: 2 day: 11.
	self assertReading: '11.2.2013' as: 'dd.m.yyyy' equals: date.
	self assertReading: '2.2013.11' as: 'm.yyyy.dd' equals: date.
	self assertReading: '11.2013.2' as: 'dd.yyyy.m' equals: date
]

{ #category : #'tests - m pattern' }
DateParsingTest >> testParsingMPatternWithTwoDigitSucceeds [ 
	| date |
	date := Date year: 2013 month: 11 day: 29.
	self assertReading: '29.11.2013' as: 'dd.m.yyyy' equals: date.
	self assertReading: '11.2013.29' as: 'm.yyyy.dd' equals: date.
	self assertReading: '29.2013.11' as: 'dd.yyyy.m' equals: date.
]

{ #category : #tests }
DateParsingTest >> testParsingPatternWithExtraCharacter [

	self assertReading: 'a4.2.2345' as: 'ad.m.y' equals: (Date year: 2345 month: 2 day: 4). 
	self assertReading: '4.2.2345' as: 'ad.m.y' raise: DateError.
	self assertReading: 'a4.2.2345' as: 'd.m.y' raise: DateError.
]

{ #category : #tests }
DateParsingTest >> testParsingPatternWithTrailingCharacterFails [

	self assertReading: '04.02.2013trailing' as: 'd.m.y' raise: DateError
]

{ #category : #'tests - y pattern' }
DateParsingTest >> testParsingYPatternWithFiveDigits [
	self assertReading: '2.11.10000' as: 'd.mm.y' equals: (Date year: 10000 month: 11 day: 2)
]

{ #category : #'tests - y pattern' }
DateParsingTest >> testParsingYPatternWithSingleDigitReturnsASecondMillenaryYear [
	self assertReading: '2.11.3' as: 'd.mm.y' equals: (Date year: 2003 month: 11 day: 2)
]

{ #category : #'tests - y pattern' }
DateParsingTest >> testParsingYPatternWithTwoDigitsReturnsASecondMillenaryYear [
	self assertReading: '2.11.13' as: 'd.mm.y' equals: (Date year: 2013 month: 11 day: 2)
]

{ #category : #'tests - yy pattern' }
DateParsingTest >> testParsingYYPatternReturnsASecondMillenaryYear [
	self assertReading: '2.11.13' as: 'd.mm.yy' equals: (Date year: 2013 month: 11 day: 2)
]

{ #category : #'tests - yy pattern' }
DateParsingTest >> testParsingYYPatternWithWrongNumberOrDigitsShouldFails [
	self assertReading: '02.11.3' as: 'dd.mm.yy' raise: DateError.
	self assertReading: '02.11.113' as: 'dd.mm.yy' raise: DateError
]

{ #category : #'tests - yyyy pattern' }
DateParsingTest >> testParsingYYYYPatternWithWrongNumberOrDigitsShouldFails [
	self assertReading: '02.11.3' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.11.13' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.11.113' as: 'dd.mm.yyyy' raise: DateError.
	self assertReading: '02.11.10113' as: 'dd.mm.yyyy' raise: DateError
]
