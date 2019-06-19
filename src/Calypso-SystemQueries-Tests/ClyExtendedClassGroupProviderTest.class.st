Class {
	#name : #ClyExtendedClassGroupProviderTest,
	#superclass : #ClyClassGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyExtendedClassGroupProviderTest >> groupProviderClass [
	^ClyExtendedClassGroupProvider 
]

{ #category : #running }
ClyExtendedClassGroupProviderTest >> packageSampleWhichHasGroup [
	^ClyClass1FromP1 package
]
