"
I provide ""undeclared"" method group when classes include any method which uses undeclared variable
"
Class {
	#name : #ClyUndeclaredMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries'
}

{ #category : #'building groups' }
ClyUndeclaredMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'undeclared' priority: 8.4 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyUndeclaredMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyUndeclaredMethodsQuery from: aClassScope
]
