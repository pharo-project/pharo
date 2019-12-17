Class {
	#name : #RGEnsureTraitTest,
	#superclass : #RGTest,
	#instVars : [
		'environment',
		'definition'
	],
	#category : #'Ring-Tests-Core'
}

{ #category : #running }
RGEnsureTraitTest >> setUp [

	super setUp.
	environment := RGEnvironment new.
]

{ #category : #running }
RGEnsureTraitTest >> testClassTrait [
		
	"test creation of named class trait on empty environment"

	definition := environment ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait. 	

]

{ #category : #running }
RGEnsureTraitTest >> testClassTraitIfMetaclassAlreadyExists [
	"test creation of named class trait if a related metaclass already exists"

	| existing |
	existing := (environment ensureClassNamed: 'SomeTrait class') baseClass.
	self assert: existing isClass.
	self assert: existing isRingResolved not.

	definition := environment ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: definition baseTrait identicalTo: existing.
	self assert: definition baseTrait isClass not.
	self assert: definition baseTrait isMetaclass not.
	self assert: definition baseTrait isTrait.
	self assert: definition baseTrait isMetaclassTrait not.
	self assert: definition baseTrait name equals: #SomeTrait
]

{ #category : #running }
RGEnsureTraitTest >> testClassTraitIfTraitAlreadyExists [
	"test creation of named class trait if a related trait already exists"

	| existing |
	existing := (environment ensureTraitNamed: 'SomeTrait') classTrait.
	self assert: existing isMetaclassTrait.
	self assert: existing isRingResolved not.

	definition := environment ensureTraitNamed: 'SomeTrait classTrait'.
	self assert: definition identicalTo: existing.
	self assert: definition isClass not.
	self assert: definition isMetaclass not.
	self assert: definition isTrait.
	self assert: definition isMetaclassTrait.
	self assert: definition baseTrait name equals: #SomeTrait
]

{ #category : #running }
RGEnsureTraitTest >> testTrait [

	"test creation of named trait on empty environment"
		
	definition := environment ensureTraitNamed: 'SomeTrait'.
	self assert: definition isClass not. 
	self assert: definition isMetaclass not. 	
	self assert: definition isTrait. 	
	self assert: definition isMetaclassTrait not. 	
]

{ #category : #running }
RGEnsureTraitTest >> testTraitIfClassAlreadyExists [
	"test creation of named trait if a class of the realted name already exists"

	| existing |
	existing := environment ensureClassNamed: 'SomeTrait'.
	self assert: existing isClass.
	self assert: existing isRingResolved.

	definition := environment ensureTraitNamed: 'SomeTrait'.
	self assert: definition identicalTo: existing.
	self assert: definition isClass not.
	self assert: definition isMetaclass not.
	self assert: definition isTrait.
	self assert: definition isMetaclassTrait not
]

{ #category : #running }
RGEnsureTraitTest >> testTraitIfClassTraitAlreadyExists [
	"test creation of named trait if a related class trait already exists"

	| existing |
	existing := (environment ensureTraitNamed: 'SomeTrait classTrait') baseTrait.
	self assert: existing isTrait.
	self assert: existing isRingResolved not.

	definition := environment ensureTraitNamed: 'SomeTrait'.
	self assert: definition identicalTo: existing.
	self assert: definition isClass not.
	self assert: definition isMetaclass not.
	self assert: definition isTrait.
	self assert: definition isMetaclassTrait not
]
