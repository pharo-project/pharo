"
I provide unclassified method group
"
Class {
	#name : 'ClyUnclassifiedMethodGroupProvider',
	#superclass : 'ClySingleMethodGroupProvider',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'building groups' }
ClyUnclassifiedMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [

	^ClyUnclassifiedMethodGroup withMethodsFrom: aMethodQuery
]

{ #category : 'building groups' }
ClyUnclassifiedMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyUnclassifiedMethodsQuery from: aClassScope
]
