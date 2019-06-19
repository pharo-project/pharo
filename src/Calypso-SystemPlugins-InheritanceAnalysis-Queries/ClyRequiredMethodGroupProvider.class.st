"
I provide ""should be implemented"" method group which includes all abstract methods from inherited classes which are still not implemented by given classes
"
Class {
	#name : #ClyRequiredMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'building groups' }
ClyRequiredMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [

	^ClyMethodGroup named: 'should be implemented' priority: 8.5 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyRequiredMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyUnimplementedMethodsQuery from: aClassScope
]

{ #category : #testing }
ClyRequiredMethodGroupProvider >> isStatic [
	^true
]
