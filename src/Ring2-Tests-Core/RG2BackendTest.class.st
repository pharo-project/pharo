Class {
	#name : #RG2BackendTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #'as yet unclassified' }
RG2BackendTest >> backendClass [

	^ RG2EnvironmentBackend  
]

{ #category : #'as yet unclassified' }
RG2BackendTest >> testDefaultContentFor: aRG2Environment [

"	| names defaultNames kernelPackage |
	
	defaultNames := #('Behavior' 'Behavior class' 'Class' 'Class class' 'ClassDescription' 'ClassDescription class' 'Metaclass' 'Metaclass class' 'Object' 'Object class' 'ProtoObject' 'ProtoObject class').
	
	self assert: ((aRG2Environment ask packages collect: #name) includes: 'Kernel').
	kernelPackage := aRG2Environment ask packageNamed: 'Kernel'.

	names := (aRG2Environment ask behaviors collect: #name).
	self assert: (names includesAll: defaultNames asSortedCollection).
	
	self assert: (aRG2Environment ask behaviorNamed: 'ProtoObject') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ProtoObject').
	self assert: (aRG2Environment ask behaviorNamed: 'ProtoObject') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'ProtoObject class').

	self assert: (aRG2Environment ask behaviorNamed: 'Object') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ProtoObject').
	self assert: (aRG2Environment ask behaviorNamed: 'Object') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Object class').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Behavior') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'Object').
	self assert: (aRG2Environment ask behaviorNamed: 'Behavior') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Behavior class').
	
	self assert: (aRG2Environment ask behaviorNamed: 'ClassDescription') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'Behavior').
	self assert: (aRG2Environment ask behaviorNamed: 'ClassDescription') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'ClassDescription class').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ClassDescription').
	self assert: (aRG2Environment ask behaviorNamed: 'Class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Class class').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Metaclass') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ClassDescription').
	self assert: (aRG2Environment ask behaviorNamed: 'Metaclass') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass class').
	
	self assert: (aRG2Environment ask behaviorNamed: 'ProtoObject class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'Class').
	self assert: (aRG2Environment ask behaviorNamed: 'ProtoObject class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Object class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ProtoObject class').
	self assert: (aRG2Environment ask behaviorNamed: 'Object class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Behavior class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'Object class').
	self assert: (aRG2Environment ask behaviorNamed: 'Behavior class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass').
	
	self assert: (aRG2Environment ask behaviorNamed: 'ClassDescription class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'Behavior class').
	self assert: (aRG2Environment ask behaviorNamed: 'ClassDescription class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Class class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ClassDescription class').
	self assert: (aRG2Environment ask behaviorNamed: 'Class class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass').
	
	self assert: (aRG2Environment ask behaviorNamed: 'Metaclass class') superclass 
		equals: (aRG2Environment ask behaviorNamed: 'ClassDescription class').
	self assert: (aRG2Environment ask behaviorNamed: 'Metaclass class') metaClass 
		equals: (aRG2Environment ask behaviorNamed: 'Metaclass').
		
	"
]
