"
SUnit tests for Ring metaclass definitions
"
Class {
	#name : #RGMetaclassDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Core-Base'
}

{ #category : #testing }
RGMetaclassDefinitionTest >> testAsMetaclassDefinition [
	| rgClass class |
	class := Class class.
	rgClass := class asRingDefinition.

	self assert: rgClass isRingObject.
	self assert: rgClass isClass.
	self assert: rgClass name identicalTo: #'Class class'.
	self assert: rgClass category identicalTo: class category.
	self assert: rgClass classSide identicalTo: rgClass
]
