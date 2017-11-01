Class {
	#name : #AClassToBeExtractedFactory,
	#superclass : #Object,
	#traits : 'TSimpleTraitToBeExtracted',
	#classTraits : 'TSimpleTraitToBeExtracted classTrait',
	#category : 'Hermes-Tests-External-Package'
}

{ #category : #'as yet unclassified' }
AClassToBeExtractedFactory >> anObject [
	^ AClassToBeExtracted new
		aVariable: 42;
		yourself
]
