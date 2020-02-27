"
SUnit tests for Ring classtrait definitions
"
Class {
	#name : #RGMetatraitDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Core-Base'
}

{ #category : #testing }
RGMetatraitDefinitionTest >> testAsClassTraitfinition [
	| rgCTrait cTrait |
	cTrait := TSortable classTrait.
	rgCTrait := cTrait asRingDefinition.
	self assert: rgCTrait isRingObject.
	self assert: rgCTrait isTrait.
	self assert: rgCTrait name identicalTo: #'TSortable classTrait'.
	self assert: rgCTrait category identicalTo: cTrait category.
	self assert: rgCTrait classSide identicalTo: rgCTrait
]
