"
I return all test methods which are marked with <expectedFailure> pragma
"
Class {
	#name : #ClyExpectedFailedTestMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #printing }
ClyExpectedFailedTestMethods >> description [
	^'expected failures'
]

{ #category : #testing }
ClyExpectedFailedTestMethods >> selectsMethod: aMethod [
	^aMethod hasPragmaNamed: #expectedFailure
]
