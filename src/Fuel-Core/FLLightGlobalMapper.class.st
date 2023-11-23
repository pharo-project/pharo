"
I map classes, traits, and global objects that belong to Smalltalk dictionary. I collaborate in serialization default behavior. 

For example, the global Transcript as well as any class in the image, are mapped by me.
"
Class {
	#name : 'FLLightGlobalMapper',
	#superclass : 'FLMapper',
	#instVars : [
		'globals'
	],
	#category : 'Fuel-Core-Mappers',
	#package : 'Fuel-Core',
	#tag : 'Mappers'
}

{ #category : 'initialization' }
FLLightGlobalMapper >> initialize [
	super initialize.
	
	globals := self configuration globals values asIdentitySet
]

{ #category : 'mapping' }
FLLightGlobalMapper >> mapAndTrace: anObject [
	"Uses the Chain of Responsibility pattern to answer the cluster which maps with the received object."

	(globals includes: anObject) 
		ifTrue: [ self mapAndTraceByClusterName: anObject to: FLGlobalValueCluster ]
		ifFalse: [ next mapAndTrace: anObject ]
]
