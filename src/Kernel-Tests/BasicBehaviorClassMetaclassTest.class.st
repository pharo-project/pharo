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
	#category : 'Kernel-Tests-Classes'
}

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testBehaviorClassClassDescriptionMetaclassHierarchy [
	"self run: #testBehaviorClassClassDescriptionMetaclassHierarchy"
	
	self assert: Class superclass  == ClassDescription.
	self assert: Metaclass superclass == ClassDescription.

	self assert: ClassDescription superclass  == Behavior.
	self assert: Behavior superclass  = Object.

	self assert: Class class class ==  Metaclass.
	self assert: Metaclass class class  == Metaclass.
	self assert: ClassDescription class class == Metaclass.
	self assert: Behavior class class == Metaclass.




	
	
	



	
	

	
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclass [
	"self run: #testMetaclass"
	
	self assert: OrderedCollection class class == Metaclass.
	self assert: Dictionary class class == Metaclass.
	self assert: Object class class == Metaclass.




	
	
	



	
	

	
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassName [
	"self run: #testMetaclassName"

	self assert: Dictionary class  name = 'Dictionary class'.
	self assert: OrderedCollection class name = 'OrderedCollection class'.
	
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassNumberOfInstances [
	"self run: #testMetaclassNumberOfInstances"

	self assert: Dictionary class allInstances size  = 1.
	self assert: OrderedCollection class allInstances size  = 1.
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassPointOfCircularity [
	"self run: #testMetaclassPointOfCircularity"

	self assert: Metaclass class instanceCount = 1.
	self assert: Metaclass class someInstance == Metaclass.


	
	
	



	
	

	
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassSuperclass [
	"self run: #testMetaclassSuperclass"

	self assert: Dictionary class superclass == Dictionary superclass class.
	self assert: OrderedCollection class superclass == OrderedCollection superclass class.
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testMetaclassSuperclassHierarchy [
	"self run: #testMetaclassSuperclassHierarchy"

	| s |
	self assert: SequenceableCollection class instanceCount  = 1.
	self assert: Collection class instanceCount  = 1.
	self assert: Object class instanceCount  = 1.
	self assert: ProtoObject class instanceCount  = 1.
	
	s := OrderedCollection new.
	s add: SequenceableCollection class.
	s add: Collection class.
	s add: Object class.
	s add: ProtoObject class.

	s add: Class.
	s add: ClassDescription.
	s add: Behavior.
	s add: Object.
	s add: ProtoObject.

	self assert: OrderedCollection class allSuperclasses  = s.



	
	

	
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testObjectAllSubclasses [
	"self run: #testObjectAllSubclasses"

	| n2 |
	n2 := Object allSubclasses size.
	self assert: n2 = (Object allSubclasses
			select: [:cls | cls class class == Metaclass
					or: [cls class == Metaclass]]) size
]

{ #category : #tests }
BasicBehaviorClassMetaclassTest >> testSuperclass [
	"self debug: #testSuperclass"

	| s b |
	s := OrderedCollection new.
	b := [:cls | cls ifNotNil: [s add: cls. b value: cls superclass] ].
	b value: OrderedCollection.

	self assert: OrderedCollection allSuperclasses = s allButFirst.
	self assert: OrderedCollection withAllSuperclasses = s.
]
