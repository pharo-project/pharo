"
I provide unclassified method group
"
Class {
	#name : #ClyUnclassifiedMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyUnclassifiedMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
		
	^ClyNoTagMethodGroup withMethodsFrom: aMethodQuery
]

{ #category : #'building groups' }
ClyUnclassifiedMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyUnclassifiedMethods from: aClassScope
]
