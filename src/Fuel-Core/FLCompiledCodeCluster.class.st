"
I am an abstract class to hold all the common behavior between the serialization / materialization of CompiledBlocks and CompiledMethods
"
Class {
	#name : #FLCompiledCodeCluster,
	#superclass : #FLIteratingCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #protected }
FLCompiledCodeCluster >> afterMaterializationStepWith: aDecoder [

	"In the aftermath of the materialization I fix all the class variables to point to the correct ones"
	objects do: [ :aCompiledCode | self fixClassVariableReferencesOf: aCompiledCode ]
]

{ #category : #materializing }
FLCompiledCodeCluster >> fixClassVariableReferencesOf: aCompiledCode [

	"When the literals are serialized / materialized the ClassVariables and GlobalVariables are handled as simples associations, but they should be recovered to point to the correct classVariable"

	1 to: aCompiledCode numLiterals do: [ :anIndex | 
		| aLiteral |
		aLiteral := aCompiledCode literalAt: anIndex.
		((aLiteral isKindOf: ClassVariable) or:[aLiteral isKindOf: GlobalVariable])
			ifTrue: [ aCompiledCode
					literalAt: anIndex
					put: (aCompiledCode methodClass bindingOf: aLiteral name) ] ]
]

{ #category : #protected }
FLCompiledCodeCluster >> materializeLiteralsTo: compiledMethod with: aDecoder [

	1 to: compiledMethod numLiterals 
		do: [ :index | compiledMethod literalAt: index put: aDecoder nextEncodedReference ].
		
]

{ #category : #'serialize/materialize' }
FLCompiledCodeCluster >> materializeReferencesStepWith: aDecoder [

	objects do: [ :aCompiledMethod | self materializeLiteralsTo: aCompiledMethod with: aDecoder ]
]

{ #category : #analyzing }
FLCompiledCodeCluster >> referencesOf: aCompiledMethod do: aBlock [

	aCompiledMethod fuelPrepare.
	1 to: aCompiledMethod numLiterals
		do: [ :index | aBlock value: (aCompiledMethod literalAt: index) ]
]

{ #category : #protected }
FLCompiledCodeCluster >> serializeLiteralsOf: aCompiledMethod with: anEncoder [

	aCompiledMethod fuelPrepare.
	1 to: aCompiledMethod numLiterals 
		do: [ :index | anEncoder encodeReferenceTo: (aCompiledMethod literalAt: index) ]
]

{ #category : #'serialize/materialize' }
FLCompiledCodeCluster >> serializeReferencesStepWith: anEncoder [

	objects do: [ :aCompiledMethod | self serializeLiteralsOf: aCompiledMethod with: anEncoder ]
]
