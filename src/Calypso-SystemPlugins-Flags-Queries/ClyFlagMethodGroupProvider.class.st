"
I provide ""flags"" method groups when given classes define methods with flag
"
Class {
	#name : #ClyFlagMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Flags-Queries'
}

{ #category : #'building groups' }
ClyFlagMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'flags' priority: 8.3 on: aMethodQuery
]

{ #category : #'building groups' }
ClyFlagMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyFlaggingMethodsQuery from: aClassScope
]
