"
I test the serialization of an entire trait, in the cases in which it *isnot* present in the image at materialization time.
"
Class {
	#name : 'FLCreateTraitSerializationTest',
	#superclass : 'FLTraitSerializationTest',
	#traits : 'FLTCreateClassOrTraitSerializationTest',
	#classTraits : 'FLTCreateClassOrTraitSerializationTest classTrait',
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'class-factory' }
FLCreateTraitSerializationTest >> newAnonymousClassOrTrait [
	^ self classFactory newAnonymousTrait
]

{ #category : 'class-factory' }
FLCreateTraitSerializationTest >> newInstanceFrom: aClassOrTrait [

	^ (aClassOrTrait isTrait
		   ifTrue: [ self classFactory silentlyMake: [ :aBuilder | aBuilder traitComposition: aClassOrTrait ] ]
		   ifFalse: [ aClassOrTrait ]) new
]
