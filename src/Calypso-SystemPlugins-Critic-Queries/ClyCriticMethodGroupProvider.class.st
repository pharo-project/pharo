"
I provide ""critiques"" method group which include all problem methods
"
Class {
	#name : #ClyCriticMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'building groups' }
ClyCriticMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	
	^ClyAllCritiquesMethodGroup 
		withMethods: aMethodQuery actualQuery from: aClassScope
]

{ #category : #'building groups' }
ClyCriticMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^(ClyAllProblemMethodQuery from: aClassScope) async
]
