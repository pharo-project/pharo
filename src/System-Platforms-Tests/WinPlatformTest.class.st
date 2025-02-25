Class {
	#name : 'WinPlatformTest',
	#superclass : 'TestCase',
	#category : 'System-Platforms-Tests-Windows',
	#package : 'System-Platforms-Tests',
	#tag : 'Windows'
}

{ #category : 'tests' }
WinPlatformTest >> testGetTempPathFromTMP [

	| value |
	OSPlatform current isWindows ifFalse: [ ^ self skip ].

	value := OSEnvironment current at: 'TMP' ifAbsent: [ nil ].
	[
	| actual expected |
	expected := 'X:\Temp\'.
	OSEnvironment current at: 'TMP' put: expected.
	actual := OSPlatform current getTempPath.
	self assert: actual equals: expected ] ensure: [
		value
			ifNil: [ OSEnvironment current removeKey: 'TMP' ]
			ifNotNil: [ OSEnvironment current at: 'TMP' put: value ] ]
]
