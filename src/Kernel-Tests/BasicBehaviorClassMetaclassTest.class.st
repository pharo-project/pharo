"
This class contains some tests regarding the classes 
	Behavior
		ClassDescription
			Class
			Metaclass
 
"
Class {
	#name : #BasicBehaviorClassMetaclassTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testBehaviorClassClassDescriptionMetaclassHierarchy [
	self assert: Class superclass identicalTo: ClassDescription.
	self assert: Metaclass superclass identicalTo: ClassDescription.

	self assert: ClassDescription superclass identicalTo: Behavior.
	self assert: Behavior superclass equals: Object.

	self assert: Class class class identicalTo: Metaclass.
	self assert: Metaclass class class identicalTo: Metaclass.
	self assert: ClassDescription class class identicalTo: Metaclass.
	self assert: Behavior class class identicalTo: Metaclass
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclass [
	self assert: OrderedCollection class class identicalTo: Metaclass.
	self assert: Dictionary class class identicalTo: Metaclass.
	self assert: Object class class identicalTo: Metaclass
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassName [

	self assert: Dictionary class name equals: 'Dictionary class'.
	self assert: OrderedCollection class name equals: 'OrderedCollection class'
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassNumberOfInstances [

	self assert: Dictionary class allInstances size equals: 1.
	self assert: OrderedCollection class allInstances size equals: 1
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassPointOfCircularity [
	self assert: Metaclass class instanceCount equals: 1.
	self assert: Metaclass class someInstance identicalTo: Metaclass
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassSuperclass [
	self assert: Dictionary class superclass identicalTo: Dictionary superclass class.
	self assert: OrderedCollection class superclass identicalTo: OrderedCollection superclass class
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassSuperclassHierarchy [
	| s |
	self assert: SequenceableCollection class instanceCount equals: 1.
	self assert: Collection class instanceCount equals: 1.
	self assert: Object class instanceCount equals: 1.
	self assert: ProtoObject class instanceCount equals: 1.
	
	s := (OrderedCollection new)
				add: SequenceableCollection class;
				add: Collection class;
				add: Object class;
				add: ProtoObject class;

				add: Class;
				add: ClassDescription;
				add: Behavior;
				add: Object;
				add: ProtoObject;
				yourself.

	self assert: OrderedCollection class allSuperclasses equals: s

	
	

	
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testObjectAllSubclasses [
	| n2 |
	n2 := Object allSubclasses size.
	self assert: n2 equals: (Object allSubclasses select: [ :cls | (cls class isKindOf: Metaclass) or: [ cls isKindOf: Metaclass ] ]) size
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testSuperclass [
	| s b |
	s := OrderedCollection new.
	b := [ :cls | 
	cls
		ifNotNil: [ s add: cls.
			b value: cls superclass ] ].
	b value: OrderedCollection.

	self assert: OrderedCollection allSuperclasses equals: s allButFirst.
	self assert: OrderedCollection withAllSuperclasses equals: s
]
