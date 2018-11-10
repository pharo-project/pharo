Class {
	#name : #ClyBreakpointMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Breakpoints'
}

{ #category : #running }
ClyBreakpointMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyClassWithHalts
]

{ #category : #running }
ClyBreakpointMethodGroupProviderTests >> groupProviderClass [
	^ClyBreakpointMethodGroupProvider
]
