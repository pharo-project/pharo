Class {
	#name : #RGBackendTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #'as yet unclassified' }
RGBackendTest >> backendClass [

	^ RGEnvironmentBackend  
]

{ #category : #'as yet unclassified' }
RGBackendTest >> testDefaultContentFor: anRGEnvironment [

"	| names defaultNames kernelPackage |
	
	defaultNames := #('Behavior' 'Behavior class' 'Class' 'Class class' 'ClassDescription' 'ClassDescription class' 'Metaclass' 'Metaclass class' 'Object' 'Object class' 'ProtoObject' 'ProtoObject class').
	
	self assert: ((anRGEnvironment ask packages collect: #name) includes: 'Kernel').
	kernelPackage := anRGEnvironment ask packageNamed: 'Kernel'.

	names := (anRGEnvironment ask behaviors collect: #name).
	self assert: (names includesAll: defaultNames asSortedCollection).
	
	self assert: (anRGEnvironment ask behaviorNamed: 'ProtoObject') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ProtoObject').
	self assert: (anRGEnvironment ask behaviorNamed: 'ProtoObject') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'ProtoObject class').

	self assert: (anRGEnvironment ask behaviorNamed: 'Object') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ProtoObject').
	self assert: (anRGEnvironment ask behaviorNamed: 'Object') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Object class').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Behavior') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'Object').
	self assert: (anRGEnvironment ask behaviorNamed: 'Behavior') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Behavior class').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'ClassDescription') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'Behavior').
	self assert: (anRGEnvironment ask behaviorNamed: 'ClassDescription') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'ClassDescription class').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ClassDescription').
	self assert: (anRGEnvironment ask behaviorNamed: 'Class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Class class').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Metaclass') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ClassDescription').
	self assert: (anRGEnvironment ask behaviorNamed: 'Metaclass') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass class').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'ProtoObject class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'Class').
	self assert: (anRGEnvironment ask behaviorNamed: 'ProtoObject class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Object class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ProtoObject class').
	self assert: (anRGEnvironment ask behaviorNamed: 'Object class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Behavior class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'Object class').
	self assert: (anRGEnvironment ask behaviorNamed: 'Behavior class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'ClassDescription class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'Behavior class').
	self assert: (anRGEnvironment ask behaviorNamed: 'ClassDescription class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Class class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ClassDescription class').
	self assert: (anRGEnvironment ask behaviorNamed: 'Class class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass').
	
	self assert: (anRGEnvironment ask behaviorNamed: 'Metaclass class') superclass 
		equals: (anRGEnvironment ask behaviorNamed: 'ClassDescription class').
	self assert: (anRGEnvironment ask behaviorNamed: 'Metaclass class') metaClass 
		equals: (anRGEnvironment ask behaviorNamed: 'Metaclass').
		
	"
]
