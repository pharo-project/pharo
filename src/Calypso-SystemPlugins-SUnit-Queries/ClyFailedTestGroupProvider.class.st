"
I provide ""failed tests"" method group when there are failed tests in given test cases
"
Class {
	#name : #ClyFailedTestGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #'building groups' }
ClyFailedTestGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'failed tests' priority: -1 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyFailedTestGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyFailedTestMethodsQuery from: aClassScope
]

{ #category : #testing }
ClyFailedTestGroupProvider >> isStatic [
	^true
]
