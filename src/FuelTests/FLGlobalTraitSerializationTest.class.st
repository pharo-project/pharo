"
I test the serialization of traits as *external* objects, i.e. the traits has to be present in the image at materialization time.
"
Class {
	#name : #FLGlobalTraitSerializationTest,
	#superclass : #FLTraitSerializationTest,
	#traits : 'FLTGlobalClassOrTraitSerializationTest',
	#classTraits : 'FLTGlobalClassOrTraitSerializationTest classTrait',
	#category : 'FuelTests'
}
