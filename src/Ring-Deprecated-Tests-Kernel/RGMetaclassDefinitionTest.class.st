"
SUnit tests for Ring metaclass definitions
"
Class {
	#name : #RGMetaclassDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Deprecated-Tests-Kernel'
}

{ #category : #deprecation }
RGMetaclassDefinitionTest class >> isDeprecated [
	^true
]

{ #category : #testing }
RGMetaclassDefinitionTest >> testAsMetaclassDefinition [
	| rgClass class |
	class := Class class.
	rgClass:= class asRingDefinition.
	
	self assert: (rgClass isRingObject).
	self assert: (rgClass isClass).  
	self assert: (rgClass name == #'Class class').  
	self assert: (rgClass category == class category).
	self assert: (rgClass classSide == rgClass).

]
