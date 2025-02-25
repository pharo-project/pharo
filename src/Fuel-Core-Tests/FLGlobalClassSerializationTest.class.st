"
I test the serialization of classes as *global* objects, i.e. the classes has to be present in the image at materialization time.
"
Class {
	#name : 'FLGlobalClassSerializationTest',
	#superclass : 'FLClassSerializationTest',
	#traits : 'FLTGlobalClassOrTraitSerializationTest',
	#classTraits : 'FLTGlobalClassOrTraitSerializationTest classTrait',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLGlobalClassSerializationTest >> testInstanceOfAnObsoleteClass [
	"Tests that serializer does not serialize an instance of an obsolete class."
	
	| aClass instance |
	aClass := self classFactory silentlyNewClass.
	instance := self newInstanceFrom: aClass.
	self classFactory delete: aClass.

	self 
		should: [self serialize: instance ] 
		raise: FLObsolete
		whoseDescriptionIncludes: aClass name
		description: 'Should raise an error when serializing as global an obsolete class.'
]
