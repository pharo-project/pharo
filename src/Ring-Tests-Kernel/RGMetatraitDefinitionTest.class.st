"
SUnit tests for Ring classtrait definitions
"
Class {
	#name : #RGMetatraitDefinitionTest,
	#superclass : #TestCase,
	#category : #Ring-Tests-Kernel
}

{ #category : #testing }
RGMetatraitDefinitionTest >> testAsClassTraitfinition [
	| rgCTrait cTrait |
	cTrait := TClass classTrait.
	rgCTrait := cTrait asRingDefinition.
	self assert: rgCTrait isRingObject.
	self assert: rgCTrait isTrait.
	self assert: rgCTrait name == #'TClass classTrait'.
	self assert: rgCTrait category == cTrait category.
	self assert: rgCTrait theMetaClass == rgCTrait
]
