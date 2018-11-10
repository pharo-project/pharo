Class {
	#name : #ClyNoTagClassGroupProviderTests,
	#superclass : #ClyClassGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyNoTagClassGroupProviderTests >> groupProviderClass [
	^ClyNoTagClassGroupProvider
]

{ #category : #running }
ClyNoTagClassGroupProviderTests >> packageSampleWhichHasGroup [
	^ClyClass6UntaggedFromP5 package "P5WithTags" 
]
