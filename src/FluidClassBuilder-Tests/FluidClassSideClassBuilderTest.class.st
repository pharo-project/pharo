Class {
	#name : #FluidClassSideClassBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassBuilder-Tests-Base'
}

{ #category : #initialization }
FluidClassSideClassBuilderTest >> setUp [

	super setUp.
	"as we can see the superclass and name are the ones of the class (not the metaclass)"
	builder := FluidClassSideClassBuilder new
		           superclassToBuild: Object;
		           nameToBuild: #Point33;
		           package: 'FakedPackage'
]

{ #category : #initialization }
FluidClassSideClassBuilderTest >> testBuilderClassName [
	
	"Yes a class side builder expects the instance name."
	self assert: builder nameToBuild equals: #'Point33'
	

]

{ #category : #initialization }
FluidClassSideClassBuilderTest >> testBuilderSuperclass [
	
	self assert: builder superclassToBuild equals: Object
	

]

{ #category : #initialization }
FluidClassSideClassBuilderTest >> testBuilderWithSlots [

	builder slots: { #metaX . #metaY }.
	"the builder is class side builder so we are setting the class level"
	
	self assert: builder classSlotsToBuild equals: {#metaX => InstanceVariableSlot. #metaY => InstanceVariableSlot}.
	self assert: builder slotsToBuild equals: #().
]

{ #category : #initialization }
FluidClassSideClassBuilderTest >> testBuilderWithTraits [

	builder traits: { TViewModelMock classTrait }.
	"the builder is class side builder so we are setting the class level"
	self assert: builder traitsToBuild isEmpty.
	self assert: builder classTraitsToBuild name equals: TViewModelMock classTrait name
]
