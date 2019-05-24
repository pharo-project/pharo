"
I wrap actual object retrieved by query and extend it with information required for the browser:
- name of item
- type of item (a class)
- position inside result items
- depth inside items hierarchy (if hierarchycal result was built)
- properties about actual object

For example you can query classes from system. The result can be sorted by name. Or classes can be arranged in subclass hierarchy. 
In first case I will represent particular class with one position and zero depth. But in another case position of same class will be different and depth could be not zero.
	
Properties are represented by first class objects: subclasses of ClyProperty. To add and access them use following messages:
	- addProperty: aProperty
	- getProperty: aPropertyClass
	- getProperty: aPropertyClass ifAbsent: aBlock
	- hasProperty: aPropertyClass

There are special kind of properties for specific purposes:

There is hierarchy of item tags represented by subclasses of ClySimpleTag. They allow mark object with specific tag. For example there is ClyAbstractItemTag which is used to mark abstract classes and methods.
You can use following methods to manage tags:
	- markWith: aSimpleTagClass
	- isMarkedWith: aSimpleTagClass. It is analogue to #hasProperty:

There is special property ClyItemChildrenTag to mark object that it includes particilar kind of children. Kind of children is represented by class of children type.
For example you can mark class with methods:
	classItem markWithChildrenOf: ClyMethod.
To check that object has particular children use:
	classItem hasChildrenOf: ClyMethod

Another special kind of property is ClyItemLocalHierarchyProperty. It includes number of all local children which exists in item query result. 
For example if you look at classes as a hierarchy then you can see Object and its subclasses. This hierarchy can be limited by package scope.
So in case of one particular package Object can have 10 subclasses. But in another package it can be 30 (for example).
And ClyItemLocalHierarchyProperty represents such local hierarchy size. To access it use following methods:
	- localHierarchySize
	- localHierarchySize: count
ClyItemLocalHierarchyProperty is used by tools to organize tree view for list of items which provide local hierarchy by themselves. Item has no real list of children. But instead it knows count of internal tree. It allows tool hide right number of items when given parent node needs to be collapsed. Important condition here is that property must hold count of full subtree of local hierarchy (not just first level children).

I collect properties using environment plugins. Query result prepares own browser items lazely when they should be returned to the user:

	aBrowserItem prepareIn: environment

But actual preparation is delegated to plugins:

	environment pluginsDo: [ :each | aBrowserItem decorateBy: each].

So every plugin decorates item with properties. At the end decoration is dispatched to the item type which sends concrete typed message back to the plugin. 
For example method decoration is evaluated to:

	aPlugin decorateBrowserItem: aBrowserItem ofMethod: aBrowserItem actualObject.
	
Properties computation can be heavy process and I use special flag #isPrepared to collect them only once.

To create my instances there is class side method:

	ClyBrowserItem named: aString with: anObject

But normally you should ask anObject directly: 
	
	anObject asCalypsoBrowserItem

During instance creation I collect the type of given object:

	anObject calypsoEnvironmentType 

The idea behind item type is to separate it from object class to not depends on single implementation of particular kind of objects. For example there is CompiledMethod, RGMethodDefinition and RG2MethodDefinition. They all represent different model of methods. But for the browser it is important to work with them in same way. So all of them return ClyMethod class as calypsoEnvironmentType. It allows avoid duplication of methods by delegating actual logic to the reusable item type class. 
For example system browser defines commands to be used in context of selected methods. It uses ClyMethod type to attach commands to methods. And they available and work independently from concrete method class. 
But by default the calypso type is the class of object. And it is not necessery to introduce separate type for every kind of item.

I provide several methods to compare items: 

- isSameAs: anotherBrowserItem
It checks that receiver and argument represent same actual object. The #same meaning is based on item type to which I delegate actual comparison:

	type checkCalypsoItem: actualObject isSameAs: anotherBrowserItem actualObject

- isEqualTo: anotherBrowserItem 
It checks that receiver and argument represent equal actual objects. The #equal meaning is based on item type to which I delegate actual comparison:
	type checkCalypsoItem: actualObject isEqualTo: anotherBrowserItem actualObject

- isSimilarTo: anotherBrowserItem
It is very weak comparison which return true for two equaly named items of same type. If they are named differently then isEqualTo: comparison will be result.
The method is used in browser logic to restore desired selection. For example when user selects new class which has the method similar to the prevous method selection of another class.

As you see comparison logic is also delegated to the item type. So in method example all method implementations do not need to duplicate them.

There are few other testing methods:

- representsItemOfType: itemType
It check that browser item belongs to the given type. It uses #includesBehaviour: logic.

- representsObject: anObject
It checks that browser item represents same object as argument. Actual comparison is delegated to the item type.

- representsObjectEqualTo: anObject
It checks that browser item represents object equal to argument. Actual comparison is delegated to the item type.

Internal Representation and Key Implementation Points.

    Instance Variables
	actualObject:		<Object>
	name:		<String>
	position:		<Number>
	depth:		<Number>
	type: <Class>
	properties:		<Collection of<ClyProperty>>
	isPrepared:		<Boolean>	

"
Class {
	#name : #ClyBrowserItem,
	#superclass : #Object,
	#instVars : [
		'position',
		'name',
		'depth',
		'properties',
		'actualObject',
		'type',
		'isPrepared'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyBrowserItem class >> named: aString with: anObject [
	^self new 
		actualObject: anObject;
		name: aString
]

{ #category : #'instance creation' }
ClyBrowserItem class >> with: anObject [
	^self new 
		actualObject: anObject
]

{ #category : #accessing }
ClyBrowserItem >> actualObject [
	^actualObject
]

{ #category : #accessing }
ClyBrowserItem >> actualObject: anObject [
	actualObject := anObject.
	type := anObject calypsoEnvironmentType
]

{ #category : #preparation }
ClyBrowserItem >> addProperty: aProperty [
	properties add: aProperty
]

{ #category : #preparation }
ClyBrowserItem >> adoptForNewCalypsoQuery [

	^self copy
]

{ #category : #preparation }
ClyBrowserItem >> asCalypsoBrowserItem [
	^self
]

{ #category : #copying }
ClyBrowserItem >> copy [
	| copy |
	copy := super copy.
	type prepareCalypsoItemCopy: copy.
	^copy
]

{ #category : #preparation }
ClyBrowserItem >> decorateBy: anEnvironmentPlugin [

	type decorateBrowserItem: self by: anEnvironmentPlugin
]

{ #category : #accessing }
ClyBrowserItem >> depth [
	^ depth
]

{ #category : #accessing }
ClyBrowserItem >> depth: anObject [
	depth := anObject
]

{ #category : #accessing }
ClyBrowserItem >> getProperty: propertyClass [ 

	^properties detect: [ :each | each isKindOf: propertyClass ] 
]

{ #category : #accessing }
ClyBrowserItem >> getProperty: propertyClass ifAbsent: absentBlock [

	^properties detect: [ :each | each isKindOf: propertyClass ] ifNone: absentBlock 
]

{ #category : #testing }
ClyBrowserItem >> hasChildrenOf: itemTypeClass [

	^self hasChildrenWhich: [ :childType | childType = itemTypeClass]
]

{ #category : #testing }
ClyBrowserItem >> hasChildrenWhich: aBlockWithChildType [

	^properties anySatisfy: [ :each | 
		(each isKindOf: ClyItemChildrenTag) 
			and: [ aBlockWithChildType value: each childrenType ] ] 
]

{ #category : #testing }
ClyBrowserItem >> hasProperty: propertyClass [

	^properties anySatisfy: [ :each | each isKindOf: propertyClass ]
]

{ #category : #testing }
ClyBrowserItem >> includesActualObject: anObject [
	^ self actualObject == anObject
]

{ #category : #initialization }
ClyBrowserItem >> initialize [
	super initialize.
	
	depth := 0.
	properties := OrderedCollection new.
	isPrepared := false
]

{ #category : #testing }
ClyBrowserItem >> isEqualTo: anotherBrowserItem [

	type = anotherBrowserItem type ifFalse: [ ^false ].
	
	^type checkCalypsoItem: actualObject isEqualTo: anotherBrowserItem actualObject
]

{ #category : #testing }
ClyBrowserItem >> isMarkedWith: propertyClass [

	^self hasProperty: propertyClass 
]

{ #category : #accessing }
ClyBrowserItem >> isPrepared [
	^ isPrepared
]

{ #category : #accessing }
ClyBrowserItem >> isPrepared: anObject [
	isPrepared := anObject
]

{ #category : #testing }
ClyBrowserItem >> isRoot [
	^depth = 0
]

{ #category : #testing }
ClyBrowserItem >> isSameAs: anotherBrowserItem [

	type = anotherBrowserItem type ifFalse: [ ^false ].
	
	^type checkCalypsoItem: actualObject isSameAs: anotherBrowserItem actualObject
]

{ #category : #testing }
ClyBrowserItem >> isSimilarTo: anotherBrowserItem [

	type = anotherBrowserItem type ifFalse: [ ^false ].
	
	^name = anotherBrowserItem name or: [ self isEqualTo: anotherBrowserItem ]
]

{ #category : #accessing }
ClyBrowserItem >> localHierarchySize [

	| property |
	property := self getProperty: ClyItemLocalHierarchyProperty ifAbsent: [ ^0].
	
	^property subtreeSize
]

{ #category : #accessing }
ClyBrowserItem >> localHierarchySize: childrenCount [
	childrenCount = 0 ifTrue: [ ^self ].
	
	self addProperty: (ClyItemLocalHierarchyProperty size: childrenCount).
]

{ #category : #preparation }
ClyBrowserItem >> markWith: simpleTagClass [
	self addProperty: simpleTagClass instance
]

{ #category : #preparation }
ClyBrowserItem >> markWithChildrenOf: anItemTypeClass [
	self addProperty: (ClyItemChildrenTag childrenType: anItemTypeClass)
]

{ #category : #accessing }
ClyBrowserItem >> name [
	^name
]

{ #category : #accessing }
ClyBrowserItem >> name: anObject [
	name := anObject
]

{ #category : #accessing }
ClyBrowserItem >> position [
	^ position
]

{ #category : #accessing }
ClyBrowserItem >> position: anObject [
	position := anObject
]

{ #category : #preparation }
ClyBrowserItem >> prepareIn: aNavigationEnvironment [
	isPrepared ifTrue: [ ^self ].
	
	aNavigationEnvironment pluginsDo: [ :each | 
		self decorateBy: each].
	
	isPrepared := true
	
]

{ #category : #printing }
ClyBrowserItem >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: name.
	aStream nextPut: $).
]

{ #category : #accessing }
ClyBrowserItem >> properties [
	^ properties
]

{ #category : #accessing }
ClyBrowserItem >> properties: anObject [
	properties := anObject
]

{ #category : #testing }
ClyBrowserItem >> representsItemOfType: aClass [
	^type isCalypsoItemType: aClass
]

{ #category : #testing }
ClyBrowserItem >> representsObject: anObject [
	actualObject == anObject ifTrue: [ ^true ].
	
	type = anObject calypsoEnvironmentType ifFalse: [ ^false ].
	
	^type checkCalypsoItem: actualObject isSameAs: anObject
]

{ #category : #testing }
ClyBrowserItem >> representsObjectEqualTo: anObject [
	actualObject == anObject ifTrue: [ ^true ].
	
	type = anObject calypsoEnvironmentType ifFalse: [ ^false ].
	
	^type checkCalypsoItem: actualObject isEqualTo: anObject
]

{ #category : #accessing }
ClyBrowserItem >> type [
	^type
]

{ #category : #accessing }
ClyBrowserItem >> type: aClass [
	type := aClass
]
