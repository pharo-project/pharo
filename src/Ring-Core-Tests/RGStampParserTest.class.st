Class {
	#name : 'RGStampParserTest',
	#superclass : 'RGTest',
	#category : 'Ring-Core-Tests',
	#package : 'Ring-Core-Tests'
}

{ #category : 'tests' }
RGStampParserTest >> testAuthor [

	"TODO: improve"
	self assert: (RGStampParser parseAuthorAliasFrom: 'StephaneDucasse 2015-10-11T18:13') equals: 'StephaneDucasse'
]

{ #category : 'tests' }
RGStampParserTest >> testTime [

	self assert: (RGStampParser parseTimestampFrom: '2015-10-11T18:13') equals: '2015-10-11T18:13:00' asDateAndTime
]
