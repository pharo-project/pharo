"
I provide ""breakpoints"" method group when classes include methods with breakpoints or halts
"
Class {
	#name : #ClyBreakpointMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Breakpoints'
}

{ #category : #'building groups' }
ClyBreakpointMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'breakpoints' priority: 8.4 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyBreakpointMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyActiveBreakpointsQuery from: aClassScope
]
