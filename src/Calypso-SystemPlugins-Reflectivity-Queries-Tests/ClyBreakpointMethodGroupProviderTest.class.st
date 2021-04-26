Class {
	#name : #ClyBreakpointMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Breakpoints'
}

{ #category : #running }
ClyBreakpointMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClassWithHaltsMock
]

{ #category : #running }
ClyBreakpointMethodGroupProviderTest >> groupProviderClass [
	^ClyBreakpointMethodGroupProvider
]
