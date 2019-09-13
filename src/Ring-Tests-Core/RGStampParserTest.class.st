Class {
	#name : #RGStampParserTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGStampParserTest >> testAuthor [

	"TODO: improve"
	self assert: (RGStampParser parseAuthorAliasFrom: 'StephaneDucasse 11/10/2015 18:13') equals: 'StephaneDucasse'.
	

]

{ #category : #tests }
RGStampParserTest >> testTime [

	"TODO: improve"
	self assert: (RGStampParser parseTimestampFrom: '11/10/2015 18:13') equals: '2015-11-10T18:13:00' asDateAndTime.
	

]
