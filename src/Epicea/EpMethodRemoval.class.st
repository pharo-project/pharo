"
I represent a removal of a method in a class or a trait.
"
Class {
	#name : 'EpMethodRemoval',
	#superclass : 'EpMethodChange',
	#instVars : [
		'method'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpMethodRemoval class >> method: aCompiledMethod [

	^ self basicNew
		initializeWithMethod: aCompiledMethod;
		yourself
]

{ #category : 'visitor' }
EpMethodRemoval >> accept: aVisitor [

	^ aVisitor visitMethodRemoval: self
]

{ #category : 'accessing' }
EpMethodRemoval >> affectedPackageName [

	^ method package
]

{ #category : 'accessing' }
EpMethodRemoval >> fileOutEntryOn: aWriteStream [
	"Write a code chunk to remove the receiver's method on aWriteStream"

	aWriteStream 
		nextPutAll: self behaviorAffectedName;
		nextPutAll: ' removeSelector: ';
		nextChunkPut: self selector asSymbol;
		cr
]

{ #category : 'initialization' }
EpMethodRemoval >> initializeWithMethod: aCompiledMethod [

	self initialize.
	method := aCompiledMethod asEpiceaRingDefinition
]

{ #category : 'accessing' }
EpMethodRemoval >> methodAffected [

	^ method
]

{ #category : 'accessing' }
EpMethodRemoval >> methodRemoved [

	^ method
]

{ #category : 'accessing' }
EpMethodRemoval >> protocol [

	^ method protocol
]

{ #category : 'accessing' }
EpMethodRemoval >> selector [

	^ method selector
]

{ #category : 'accessing' }
EpMethodRemoval >> sourceCode [

	^ method sourceCode
]
