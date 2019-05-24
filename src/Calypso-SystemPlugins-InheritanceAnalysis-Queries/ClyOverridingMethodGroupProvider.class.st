"
I provide ""overrides"" method group which includes all methods which are overridden by given classes
"
Class {
	#name : #ClyOverridingMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'building groups' }
ClyOverridingMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'overrides' priority: -10 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyOverridingMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyOverridingMethodsQuery from: aClassScope
]
