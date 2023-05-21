Class {
	#name : #WinPlatformTest,
	#superclass : #TestCase,
	#category : #'System-Platforms-Tests-Windows'
}

{ #category : #tests }
WinPlatformTest >> testGetTemporaryDirectoryFromTMP [

	| value |
	OSPlatform current isWindows ifFalse: [ ^ self skip ].

	value := OSEnvironment current at: 'TMP' ifAbsent: [  ].
	[
	| actual expected |
	expected := 'X:\Temp\'.
	OSEnvironment current at: 'TMP' put: expected.
	actual := OSPlatform current getTemporaryDirectory.
	self assert: actual equals: expected ] ensure: [
		value ifNotNil: [ OSEnvironment current at: 'TMP' put: value ] ]
]
