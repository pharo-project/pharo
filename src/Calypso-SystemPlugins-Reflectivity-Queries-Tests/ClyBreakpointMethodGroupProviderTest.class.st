Class {
	#name : #ClyBreakpointMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Breakpoints'
}

{ #category : #running }
ClyBreakpointMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClassWithHalts
]

{ #category : #running }
ClyBreakpointMethodGroupProviderTest >> groupProviderClass [
	^ClyBreakpointMethodGroupProvider
]
