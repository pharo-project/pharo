"
SUnit tests for class  DosTimeStamp
"
Class {
	#name : 'DosTimestampTest',
	#superclass : 'TestCase',
	#category : 'System-Time-Tests',
	#package : 'System-Time-Tests'
}

{ #category : 'tests' }
DosTimestampTest >> testAsDateAndTime [

	| timestamp |
	timestamp := DosTimestamp on: 16r40B57856.
	self assert: timestamp asDateAndTime equals: (DateAndTime fromString: '2012-05-21T15:02:44')
]

{ #category : 'tests' }
DosTimestampTest >> testFromDateAndTime [

	| timestamp |
	timestamp := DosTimestamp fromDateAndTime: (DateAndTime fromString: '2012-05-21T15:02:44pm').

	self assert: timestamp value equals: 16r40B57856
]

{ #category : 'tests' }
DosTimestampTest >> testTimesAreLocal [

	| remoteDateAndTime remoteTimestamp localTimestamp |
	remoteDateAndTime := DateAndTime current offset: DateAndTime localOffset + 2 hours.
	remoteTimestamp := DosTimestamp fromDateAndTime: remoteDateAndTime.
	localTimestamp := DosTimestamp fromDateAndTime: remoteDateAndTime asLocal.
	self assert: remoteTimestamp equals: localTimestamp
]
