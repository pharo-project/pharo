"
I map classes, traits, and global objects that belong to Smalltalk dictionary. I collaborate in serialization default behavior. 

For example, the global Transcript as well as any class in the image, are mapped by me.
"
Class {
	#name : #FLLightGlobalMapper,
	#superclass : #FLMapper,
	#instVars : [
		'globals'
	],
	#category : #'Fuel-Core-Mappers'
}

{ #category : #'instance creation' }
FLLightGlobalMapper class >> for: globalSymbols in: globalEnvironment [

	^ self basicNew
		initializeWith: (self valuesFor: globalSymbols in: globalEnvironment);
		yourself.
]

{ #category : #private }
FLLightGlobalMapper class >> valuesFor: globalSymbols in: globalEnvironment [
	"Some globals, like ActiveHand have a nil value in the Smalltalk globals. Therefore, we cannot map nil to globalCluster. We could filter before in #defaultGlobalSymbols but that means that not even the Association will be consider global."
	| values |
	values := IdentitySet new: globalSymbols size.
	"todo: optimized ugly code"
	globalSymbols 
		do: [:aSymbol | 
			(globalEnvironment 
				at: aSymbol ifPresent: [:value | 
					value ifNotNil: [ 
						values add: value ] ]) ].
	^ values
]

{ #category : #'initialize-release' }
FLLightGlobalMapper >> initializeWith: someObjects [

	self initialize.
	globals := someObjects.
]

{ #category : #mapping }
FLLightGlobalMapper >> mapAndTrace: anObject [
	"Uses the Chain of Responsibility pattern to answer the cluster which maps with the received object."

	(globals includes: anObject) 
		ifTrue: [ self mapAndTraceByClusterName: anObject to: FLGlobalValueCluster ]
		ifFalse: [ next mapAndTrace: anObject ]
]
