"
I have the common behavior for testing trait serialization.
"
Class {
	#name : 'FLTraitSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLTraitSerializationTest >> newClassOrTrait [
	^ self classFactory silentlyNewTrait
]
