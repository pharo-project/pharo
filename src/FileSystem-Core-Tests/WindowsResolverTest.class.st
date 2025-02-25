"
A WindowsResolverTest is a test class for testing the behavior of WindowsResolver
"
Class {
	#name : 'WindowsResolverTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Core-Tests-Resolver',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Resolver'
}

{ #category : 'tests' }
WindowsResolverTest >> testResolveTempPathFromTMP [

	| expected |
	OSPlatform current isWindows ifFalse: [ ^ self skip ].

	expected := 'X:\Temp'.

	OSEnvironment current setEnv: 'TMP' value: expected during: [
		| actual |
		actual := WindowsResolver new temp.
		self assert: actual fullName equals: expected ]
]
