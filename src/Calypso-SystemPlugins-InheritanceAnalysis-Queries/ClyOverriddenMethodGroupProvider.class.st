"
I provide ""overridden"" method group which includes all methods of given classes which are overridden by subclasses
"
Class {
	#name : #ClyOverriddenMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'building groups' }
ClyOverriddenMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'overridden' priority: -10 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyOverriddenMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyOverriddenMethodsQuery from: aClassScope
]
