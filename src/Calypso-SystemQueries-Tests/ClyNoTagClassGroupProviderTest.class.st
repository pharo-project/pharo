Class {
	#name : #ClyNoTagClassGroupProviderTest,
	#superclass : #ClyClassGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyNoTagClassGroupProviderTest >> groupProviderClass [
	^ClyNoTagClassGroupProvider
]

{ #category : #running }
ClyNoTagClassGroupProviderTest >> packageSampleWhichHasGroup [
	^ClyClass6UntaggedFromP5Mock package "P5WithTags" 
]
