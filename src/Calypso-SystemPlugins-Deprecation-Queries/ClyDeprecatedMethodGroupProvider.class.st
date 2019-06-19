"
I provide ""deprecated"" method groups when given classes define them
"
Class {
	#name : #ClyDeprecatedMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries'
}

{ #category : #'building groups' }
ClyDeprecatedMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'deprecated' priority: -9 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyDeprecatedMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyDeprecatedMethodsQuery from: aClassScope
]
