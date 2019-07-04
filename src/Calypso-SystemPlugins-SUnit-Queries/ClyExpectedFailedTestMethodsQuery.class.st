"
I return all test methods which are marked with <expectedFailure> pragma
"
Class {
	#name : #ClyExpectedFailedTestMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #printing }
ClyExpectedFailedTestMethodsQuery >> description [
	^'expected failures'
]

{ #category : #testing }
ClyExpectedFailedTestMethodsQuery >> selectsMethod: aMethod [
	^aMethod hasPragmaNamed: #expectedFailure
]
