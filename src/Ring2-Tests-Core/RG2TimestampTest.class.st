Class {
	#name : #RG2TimestampTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2TimestampTest >> testAuthor [

	"TODO: improve"
	self assert: (RG2ElementDefinition parseAuthorAliasFrom: 'StephaneDucasse 11/10/2015 18:13') equals: 'StephaneDucasse'.
	

]

{ #category : #tests }
RG2TimestampTest >> testTime [

	"TODO: improve"
	self assert: (RG2ElementDefinition parseTimestampFrom: '11/10/2015 18:13') equals: '2015-11-10T18:13:00' asDateAndTime.
	

]
