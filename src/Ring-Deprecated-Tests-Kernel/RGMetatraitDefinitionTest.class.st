"
SUnit tests for Ring classtrait definitions
"
Class {
	#name : #RGMetatraitDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Deprecated-Tests-Kernel'
}

{ #category : #deprecation }
RGMetatraitDefinitionTest class >> isDeprecated [
	^true
]

{ #category : #testing }
RGMetatraitDefinitionTest >> testAsClassTraitfinition [
	| rgCTrait cTrait |
	cTrait := TSortable classTrait.
	rgCTrait := cTrait asRingDefinition.
	self assert: rgCTrait isRingObject.
	self assert: rgCTrait isTrait.
	self assert: rgCTrait name == #'TSortable classTrait'.
	self assert: rgCTrait category == cTrait category.
	self assert: rgCTrait classSide == rgCTrait
]
