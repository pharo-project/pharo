Class {
	#name : #ClyNoTagClassGroupProviderTest,
	#superclass : #ClyClassGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyNoTagClassGroupProviderTest >> groupProviderClass [
	^ClyNoTagClassGroupProvider
]

{ #category : #running }
ClyNoTagClassGroupProviderTest >> packageSampleWhichHasGroup [
	^ClyClass6UntaggedFromP5 package "P5WithTags" 
]
