"
An UnixPlatformTest is a test class for testing the behavior of UnixPlatform
"
Class {
	#name : #UnixPlatformTest,
	#superclass : #TestCase,
	#category : #'System-Platforms-Tests-Unix'
}

{ #category : #tests }
UnixPlatformTest >> testGetTemporaryDirectoryFromTMPDIR [

	| value |
	OSPlatform current isUnix ifFalse: [ ^ self skip ].

	value := OSEnvironment current at: 'TMPDIR' ifAbsent: [  ].
	[
	| actual expected |
	expected := '/tmp/foo'.
	OSEnvironment current at: 'TMPDIR' put: expected.
	actual := OSPlatform current getTemporaryDirectory.
	self assert: actual equals: expected ] ensure: [
		value ifNotNil: [ OSEnvironment current at: 'TMPDIR' put: value ] ]
]
