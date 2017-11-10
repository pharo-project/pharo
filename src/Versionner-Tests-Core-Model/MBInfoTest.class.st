"
Common SUnit tests for MBInfo
"
Class {
	#name : #MBInfoTest,
	#superclass : #MBAbstractTest,
	#category : #Versionner-Tests-Core-Model
}

{ #category : #tests }
MBInfoTest >> testIsDirty [
	"info is not dirty per default"
	self deny: MBInfo new isDirty
]

{ #category : #tests }
MBInfoTest >> testName [
	| info |
	info := MBInfo new.
	self assert: (info name isNil).

	info name: 'foobar'.
	self assert: (info name = 'foobar').
	self assert: (info printString = 'foobar').


]
