"
I provide ""expected failures"" method group when test cases include tests marked with <expectedFailure> pragma
"
Class {
	#name : #ClyExpectedFailureMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #'building groups' }
ClyExpectedFailureMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'expected failures' priority: -2 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyExpectedFailureMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyExpectedFailedTestMethodsQuery from: aClassScope
]
