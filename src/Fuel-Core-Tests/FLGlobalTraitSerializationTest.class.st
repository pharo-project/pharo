"
I test the serialization of traits as *external* objects, i.e. the traits has to be present in the image at materialization time.
"
Class {
	#name : 'FLGlobalTraitSerializationTest',
	#superclass : 'FLTraitSerializationTest',
	#traits : 'FLTGlobalClassOrTraitSerializationTest',
	#classTraits : 'FLTGlobalClassOrTraitSerializationTest classTrait',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLGlobalTraitSerializationTest >> testCreateClassWithCreatedTraitWithInstanceVariable [
	"Tests materialization of a class and a trait not defined in the image.
	The class uses the trait.
	The trait has instance variables."

	| aClass aTrait result materializedClass materializedTrait materializedObject |
	aClass := self classFactory silentlyMake: [ :aBuilder | aBuilder slots: #(ivar) ].
	self classFactory silentlyCompile: 'ivar: x ivar := x' in: aClass.
	self classFactory silentlyCompile: 'ivar ^ ivar' in: aClass.
	aTrait := self classFactory silentlyMake: [ :aBuilder |
		          aBuilder
			          beTrait;
			          slots: #(traitIvar) ].
	self classFactory silentlyCompile: 'traitIvar: x traitIvar := x' in: aTrait.
	self classFactory silentlyCompile: 'traitIvar ^ traitIvar' in: aTrait.
	aClass addToComposition: aTrait.

	result := self resultOfSerializeAndMaterialize: {
			          aClass.
			          aTrait.
			          (aClass new
				           perform: #ivar: with: 1;
				           perform: #traitIvar: with: 2;
				           yourself) }.
	materializedClass := result first.
	materializedTrait := result second.
	materializedObject := result third.

	self assert: materializedClass slots size equals: 2.
	self assert: materializedTrait slots size equals: 1.
	self assert: materializedClass slots first name equals: 'ivar'.
	self assert: materializedClass slots second name equals: 'traitIvar'.
	self assert: (materializedObject perform: #ivar) equals: 1.
	self assert: (materializedObject perform: #traitIvar) equals: 2.

	"Check that the class format is correct and that the instance
	was allocated with enough space to store values."
	materializedObject
		perform: #ivar: with: 3;
		perform: #traitIvar: with: 4.
	self assert: (materializedObject perform: #ivar) equals: 3.
	self assert: (materializedObject perform: #traitIvar) equals: 4
]
