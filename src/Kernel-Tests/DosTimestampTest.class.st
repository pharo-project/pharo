"
SUnit tests for class  DosTimeStamp
"
Class {
	#name : #DosTimestampTest,
	#superclass : #TestCase,
	#category : #Kernel-Tests-Chronology
}

{ #category : #tests }
DosTimestampTest >> testAsDateAndTime [
	
	| remoteDatetime timestamp |
	timestamp := DosTimestamp on: 16r40B57856.
	self assert: timestamp asDateAndTime equals: '21 May 2012 3:02:44 pm' asDateAndTime.
]

{ #category : #tests }
DosTimestampTest >> testFromDateAndTime [

	| aDateAndTime timestamp |
	timestamp := DosTimestamp fromDateAndTime: '21 May 2012 3:02:44 pm' asDateAndTime.
	
	self assert: timestamp value equals: 16r40B57856.
]

{ #category : #tests }
DosTimestampTest >> testTimesAreLocal [

	| remoteDateAndTime remoteTimestamp localTimestamp |
	remoteDateAndTime := DateAndTime current offset: DateAndTime localOffset + 2 hours.
	remoteTimestamp := DosTimestamp fromDateAndTime: remoteDateAndTime.
	localTimestamp := DosTimestamp fromDateAndTime: remoteDateAndTime asLocal.
	self assert: remoteTimestamp equals: localTimestamp.
]
