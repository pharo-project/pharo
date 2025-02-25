"
SUnit tests for Ring metaclass definitions
"
Class {
	#name : 'RGMetaclassDefinitionTest',
	#superclass : 'TestCase',
	#category : 'Ring-Definitions-Core-Tests-Base',
	#package : 'Ring-Definitions-Core-Tests',
	#tag : 'Base'
}

{ #category : 'testing' }
RGMetaclassDefinitionTest >> testAsMetaclassDefinition [
	| rgClass class |
	class := Class class.
	rgClass := class asRingDefinition.

	self assert: rgClass isRingObject.
	self assert: rgClass isClass.
	self assert: rgClass name identicalTo: #'Class class'.
	self assert: rgClass package name identicalTo: class package name.
	self assert: rgClass packageTag identicalTo: class packageTag name.
	self assert: rgClass classSide identicalTo: rgClass
]
