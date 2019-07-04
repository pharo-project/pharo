"
I provide ""abstract"" method group which includes all abstract methods of given classes
"
Class {
	#name : #ClyAbstractMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'building groups' }
ClyAbstractMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'abstract' priority: -10 on: aMethodQuery
]

{ #category : #'building groups' }
ClyAbstractMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyAbstractMethodsQuery from: aClassScope
]
