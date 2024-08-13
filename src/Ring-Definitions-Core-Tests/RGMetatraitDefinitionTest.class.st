"
SUnit tests for Ring classtrait definitions
"
Class {
	#name : 'RGMetatraitDefinitionTest',
	#superclass : 'TestCase',
	#category : 'Ring-Definitions-Core-Tests-Base',
	#package : 'Ring-Definitions-Core-Tests',
	#tag : 'Base'
}

{ #category : 'testing' }
RGMetatraitDefinitionTest >> testAsClassTraitfinition [

	| rgCTrait cTrait |
	cTrait := TSortable classTrait.
	rgCTrait := cTrait asRingDefinition.
	self assert: rgCTrait isRingObject.
	self assert: rgCTrait isTrait.
	self assert: rgCTrait name identicalTo: #'TSortable classTrait'.
	self assert: rgCTrait package name identicalTo: cTrait package name.
	self assert: rgCTrait packageTag identicalTo: cTrait packageTag name.
	self assert: rgCTrait classSide identicalTo: rgCTrait
]
