Class {
	#name : #ClyExtendedClassGroupProviderTests,
	#superclass : #ClyClassGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyExtendedClassGroupProviderTests >> groupProviderClass [
	^ClyExtendedClassGroupProvider 
]

{ #category : #running }
ClyExtendedClassGroupProviderTests >> packageSampleWhichHasGroup [
	^ClyClass1FromP1 package
]
