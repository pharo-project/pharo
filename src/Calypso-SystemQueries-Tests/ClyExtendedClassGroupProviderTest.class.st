Class {
	#name : #ClyExtendedClassGroupProviderTest,
	#superclass : #ClyClassGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyExtendedClassGroupProviderTest >> groupProviderClass [
	^ClyExtendedClassGroupProvider 
]

{ #category : #running }
ClyExtendedClassGroupProviderTest >> packageSampleWhichHasGroup [
	^ClyClass1FromP1Mock package
]
