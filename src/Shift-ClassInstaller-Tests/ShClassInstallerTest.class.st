Class {
	#name : #ShClassInstallerTest,
	#superclass : #TestCase,
	#instVars : [
		'newClass',
		'superClass',
		'subClass',
		'newClass2',
		'superClass2'
	],
	#category : #'Shift-ClassInstaller-Tests'
}

{ #category : #'instance creation' }
ShClassInstallerTest >> generatedClassesPackageName [

	^ 'Shift-ClassInstallerTests-GeneratedPackage'
]

{ #category : #'instance creation' }
ShClassInstallerTest >> newClass: className slots: slots [
	^ self newClass: className superclass: Object slots: slots
]

{ #category : #'instance creation' }
ShClassInstallerTest >> newClass: className slots: slots metaclassSlots: metaclassSlots [

	^ self newClass: className superclass: Object  slots: slots metaclassSlots: metaclassSlots
]

{ #category : #'instance creation' }
ShClassInstallerTest >> newClass: className superclass: aSuperclass slots: slots [

	^ self newClass: className superclass: aSuperclass  slots: slots metaclassSlots: #()
]

{ #category : #'instance creation' }
ShClassInstallerTest >> newClass: className superclass: aSuperclass slots: slots metaclassSlots: metaclassSlots [

	^ ShiftClassInstaller make: [ :builder |
		  builder
			  name: className;
			  superclass: aSuperclass;
			  slots: slots;
			  classSlots: metaclassSlots;
			  sharedVariables: '';
			  sharedPools: '';
			  category: self generatedClassesPackageName ]
]

{ #category : #running }
ShClassInstallerTest >> tearDown [

	(self generatedClassesPackageName asPackageIfAbsent: [ nil ]) ifNotNil: [ :package | package removeFromSystem ].
	super tearDown
]

{ #category : #tests }
ShClassInstallerTest >> testBuildingClassesWithSlotsClassifiesItsMethods [
	"This is a regression test. 
	What was happening before was that the slots were generating a method to initialize themselves and after that the old class organization was copied. The problem is that during the first class creation, this was reseting the protocol of the generated slot methods."

	newClass := self newClass: #ShCITestClass slots: { (#anInstanceVariable => BooleanSlot) }.

	self assertCollection: newClass selectors hasSameElements: { #initialize }.
	self assert: (newClass protocolOfSelector: #initialize) isNotNil.

	newClass2 := newClass duplicateClassWithNewName: #ShCITestClass2.

	self assertCollection: newClass2 selectors hasSameElements: { #initialize }.
	self assert: (newClass2 protocolOfSelector: #initialize) isNotNil
]

{ #category : #tests }
ShClassInstallerTest >> testChangingASharedPoolUpdatesCorrectlyUsers [

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #ShCITestSharedPool;
			            superclass: SharedPool;
			            sharedVariables: #( A B C );
			            category: self generatedClassesPackageName ].

	newClass2 := ShiftClassInstaller make: [ :builder |
		             builder
			             name: #ShCITestClass;
			             sharedPools: 'ShCITestSharedPool';
			             category: self generatedClassesPackageName ].

	newClass2 compile: 'a ^A'.

	self assert: ((newClass2 >> #a) literals at: 1) identicalTo: (newClass classVariableNamed: #A).

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #ShCITestSharedPool;
			            superclass: SharedPool;
			            sharedVariables: #( A B C D );
			            category: self generatedClassesPackageName ].

	self assert: ((newClass2 >> #a) literals at: 1) identicalTo: (newClass classVariableNamed: #A)
]

{ #category : #tests }
ShClassInstallerTest >> testChangingHierarchy [
	"Testing that the changes in the superclasses are propagated to the subclasses"

	superClass := self newClass:#ShCITestClass1 slots:#().
	newClass := self newClass:#ShCITestClass2 superclass: superClass slots:#(aSlot).
	self assert: newClass classLayout slotScope parentScope equals: superClass classLayout slotScope.

	superClass2 := self newClass:#ShCITestClass0 slots:#(anotherSlot).
	superClass := self newClass:#ShCITestClass1 superclass: superClass2 slots:#().

	self assert: newClass classLayout slotScope parentScope equals: superClass classLayout slotScope.
	self assert: superClass classLayout slotScope parentScope equals: superClass2 classLayout slotScope.
	self assertCollection: (newClass allSlots collect: [:each | each name]) hasSameElements: #(aSlot anotherSlot)
]

{ #category : #tests }
ShClassInstallerTest >> testChangingSlotCallsInitializeSlotOnInstanceMigration [

	| className someInstance |
	className := #ShClassWithNormalSlots.
	newClass := self newClass: className slots:#( someSlot ).
	someInstance := newClass	new.

	"Migrate the class changing the slot definition"
	self newClass: className slots: { #someSlot => ShTestSlot }.

	"The slot should have initialized the value"
	self assert: (someInstance readSlotNamed: #someSlot) equals: 'initialized'
]

{ #category : #tests }
ShClassInstallerTest >> testChangingSuperclassInTheHierarchy [

	superClass := self newClass:#ShCITestClass slots:#(var1 var2).
	superClass2 := self newClass:#ShCITestClass2 slots:#().
	subClass := self newClass:#ShCISubTestClass superclass: superClass2 slots:#().

   superClass2 := self newClass:#ShCITestClass2 superclass: superClass slots:#().

	self deny: superClass subclasses isEmpty.
	self assertCollection: superClass2 subclasses equals: { subClass }.
	self assertCollection: superClass subclasses equals: { superClass2 }.
	self assert: subClass superclass equals: superClass2.
	self assert: superClass2 superclass equals: superClass
]

{ #category : #tests }
ShClassInstallerTest >> testChangingSuperclassToOther [
	superClass := self newClass: #ShCITestClass slots: #(var1).
	superClass2 := self newClass: #ShCITestClass2 slots: #().
	subClass := self newClass: #ShCISubTestClass superclass: superClass2 slots: #().

	subClass := self newClass: #ShCISubTestClass superclass: superClass slots: #().

	superClass2 := self newClass: #ShCITestClass2 superclass: superClass slots: #().

	self denyEmpty: superClass subclasses.
	self assertEmpty: superClass2 subclasses.
	self assertCollection: superClass subclasses equals: {subClass . superClass2}.
	self assert: subClass superclass equals: superClass.
	self assert: superClass2 superclass equals: superClass
]

{ #category : #tests }
ShClassInstallerTest >> testClassWithComment [

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #SHClassWithComment;
			            superclass: Object;
			            slots: #(  );
			            sharedVariables: '';
			            sharedPools: '';
			            category: self generatedClassesPackageName;
			            comment: 'I have a comment';
			            commentStamp: 'anStamp' ].

	self assert: newClass comment equals: 'I have a comment'.
	self assert: newClass commentStamp equals: 'anStamp'
]

{ #category : #tests }
ShClassInstallerTest >> testClassWithSlotCallsInitializeSlotOnInstanceCreation [

	| className someInstance |
	className := #ShClassWithNormalSlots.
	newClass := self newClass: className slots: { #someSlot => ShTestSlot }.

	someInstance := newClass	new.

	"The slot should have initialized the value"
	self assert: (someInstance readSlotNamed: #someSlot) equals: 'initialized'
]

{ #category : #tests }
ShClassInstallerTest >> testClassWithSlotHasInitializeMethodWithInitializeSlots [

	| className |
	className := #ShClassWithNormalSlots.
	newClass := self newClass: className slots: { #someSlot => ShTestSlot }.

	self assert: ((newClass >> #initialize) ast statements
		includes: (RBParser parseExpression: 'self class initializeSlots: self'))
]

{ #category : #tests }
ShClassInstallerTest >> testDuplicateClassPreserveClassSlots [

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #ShCITestClass;
			            superclass: Object;
			            classSlots: { (#aClassInstanceVariable => InstanceVariableSlot) };
			            category: self generatedClassesPackageName ].

	newClass2 := newClass duplicateClassWithNewName: #ShCITestClass2.

	self assert: (newClass2 class hasSlotNamed: #aClassInstanceVariable).
	self assert: (newClass2 class slotNamed: #aClassInstanceVariable) class equals: InstanceVariableSlot
]

{ #category : #tests }
ShClassInstallerTest >> testDuplicateClassPreserveMethods [

	newClass := self newClass: #ShCITestClass slots: {#anInstanceVariable => BooleanSlot}.
	newClass compile: 'm1 ^ 42'.
	newClass class compile: 'm2 ^ 42'.

	newClass2 := newClass duplicateClassWithNewName: #ShCITestClass2.

	self assert: (newClass2 includesSelector: #m1).
	self assert: (newClass2 class includesSelector: #m2)
]

{ #category : #tests }
ShClassInstallerTest >> testDuplicateClassPreserveSlots [

	newClass := self newClass: #ShCITestClass slots: {#anInstanceVariable => BooleanSlot}.
	newClass2 := newClass duplicateClassWithNewName: #ShCITestClass2.

	self assert: (newClass2 hasSlotNamed: #anInstanceVariable).
	self assert: (newClass2 slotNamed: #anInstanceVariable) class equals: BooleanSlot
]

{ #category : #tests }
ShClassInstallerTest >> testDuplicatedInstanceVariable [
	superClass := self newClass: #ShCITestClass1 slots: #(aSlot).
	newClass := self newClass: #ShCITestClass2 superclass: superClass slots: #().

	"The DuplicatedSlotName should not be resumable. If it is resume, the process will generate
   duplicated instance variables. The shadowing of IV is not possible in Pharo."
	[ newClass := self newClass: #ShCITestClass2 superclass: superClass slots: #(aSlot).
	self fail ]
		on: DuplicatedSlotName
		do: [ :ex | self deny: ex isResumable ]
]

{ #category : #tests }
ShClassInstallerTest >> testDuplicatedInstanceVariableInSuperclass [

	superClass := self newClass: #ShCITestClass1 slots: #().
	newClass := self newClass: #ShCITestClass2 superclass: superClass slots: #(aSlot).

	[ superClass := self newClass: #ShCITestClass1 slots: #(aSlot).
	self fail ]
		on: DuplicatedSlotName
		do: [ :ex | self deny: ex isResumable ].
	self deny: ((superClass slots collect: [:e | e name]) includes: 'aSlot')
]

{ #category : #tests }
ShClassInstallerTest >> testDuplicatedInstanceVariableInSuperclassMetaclass [

	superClass := self newClass: #ShCITestClass1 slots: #() metaclassSlots: #().
	newClass := self
		newClass: #ShCITestClass2 superclass: superClass
		slots: #() metaclassSlots: #(aSlot).

	[ superClass := self newClass: #ShCITestClass1 slots: #() metaclassSlots: #(aSlot).
	self fail ]
		on: DuplicatedSlotName
		do: [ :ex | self deny: ex isResumable ].
	self deny: ((superClass class slots collect: [:e | e name]) includes: 'aSlot')
]

{ #category : #tests }
ShClassInstallerTest >> testInstallInSpecificEnvironment [

	| newEnvironment |
	newEnvironment := SystemDictionary new.
	newClass := ShiftClassInstaller new
		            installingEnvironment: (ShSmalltalkGlobalsEnvironment new
				             environment: newEnvironment;
				             yourself);
		            make: [ :builder |
			            builder
				            name: #SHClassForTest;
				            superclass: Object;
				            category: self generatedClassesPackageName ].

	self class environment at: #SHClassForTest ifPresent: [ self fail: 'The default environment should not have the class installed.' ].
	newEnvironment at: #SHClassForTest ifAbsent: [ self fail: 'The default environment should not have the class installed.' ].
	
	self skip. 
	self flag: #package. "ShiftClassInstaller>>#recategorize:to: is using the old SystemOrganizer system to classify classes, but this does not add the classes to the instance of RPackageOrganizer but to the default package organizer of the image instead. Until we can fix this, we cannot have the next assertions working. They are clearly a bug but it'll be hard to fix this bug in short term........ :("
	self deny: (self packageOrganizer hasPackage: self generatedClassesPackageName).
	self assert: (newEnvironment organization hasPackage: self generatedClassesPackageName)
]

{ #category : #tests }
ShClassInstallerTest >> testModifyingClassKeepsOrganizationOfMethods [
	newClass := self newClass: #ShCITestClass superclass: subClass slots: #().

	newClass compile: 'aMethod ^ 42' classified: #'useful-message' notifying: nil.
	newClass class compile: 'aClassMethod ^ 21' classified: #'useful-message' notifying: nil.

	self assert: (newClass >> #aMethod) protocolName equals: #'useful-message'.
	self assert: (newClass class >> #aClassMethod) protocolName equals: #'useful-message'.

	newClass := self newClass: #ShCITestClass superclass: subClass slots: #(aSlot).

	self assert: (newClass >> #aMethod) protocolName equals: #'useful-message'.
	self assert: (newClass class >> #aClassMethod) protocolName equals: #'useful-message'
]

{ #category : #tests }
ShClassInstallerTest >> testModifyingClassSideInstances [
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable).
	newClass := self newClass: #ShCITestSubClass superclass: superClass slots: #().

	superClass class addInstVarNamed: #aVariable.

	self assert: (superClass class hasSlotNamed: #aVariable).
	self assert: (newClass class hasSlotNamed: #aVariable)
]

{ #category : #tests }
ShClassInstallerTest >> testModifyingSuperclass [
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable).
	newClass := self newClass: #ShCITestSubClass superclass: superClass slots: #().

	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.

	superClass := self newClass:#ShCITestClass slots: #(anInstanceVariable otherVariable).

	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.
	superClass new instVarNamed: #otherVariable.
	newClass new instVarNamed: #otherVariable
]

{ #category : #tests }
ShClassInstallerTest >> testModifyingSuperclassInOtherOrder [
	| obj |
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable).
	subClass := self newClass:#ShCISubTestClass superclass: superClass slots:#(anSubInstanceVariable).
	newClass := self newClass: #ShCITestSubClass superclass: subClass slots: #().

	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.

	obj := newClass new.
	obj instVarNamed: #anInstanceVariable put: 7.
	obj instVarNamed: #anSubInstanceVariable put: 17.

	superClass := self newClass:#ShCITestClass slots:#(otherVariable anInstanceVariable).

	self assert: (obj instVarNamed: #anInstanceVariable) equals: 7.
	self assert: (obj instVarNamed: #anSubInstanceVariable) equals: 17
]

{ #category : #tests }
ShClassInstallerTest >> testTryingToModifyReadOnlyInstances [
	| obj obj2 |
	newClass := self newClass: #ShCITestClass slots: #(anInstanceVariable).

	obj := newClass new.
	obj instVarNamed: #anInstanceVariable put: 7.
	obj beReadOnlyObject.

	obj2 := newClass new.
	obj2 instVarNamed: #anInstanceVariable put: 17.

	self newClass:#ShCITestClass slots:#(otherVariable anInstanceVariable).

	self assert: obj isReadOnlyObject
]

{ #category : #tests }
ShClassInstallerTest >> testTryingToModifyReadOnlySubInstances [
	| obj obj2 |
	superClass := self newClass: #ShCITestSuperClass slots: #(aSuperVariable).
	newClass := self newClass: #ShCITestClass superclass: superClass slots: #(anInstanceVariable).

	obj := newClass new.
	obj instVarNamed: #aSuperVariable put: 1.
	obj instVarNamed: #anInstanceVariable put: 7.
	obj beReadOnlyObject.

	obj2 := newClass new.
	obj2 instVarNamed: #aSuperVariable put: 11.
	obj2 instVarNamed: #anInstanceVariable put: 17.

	self newClass:#ShCITestClass slots:#(otherVariable anInstanceVariable).

	self assert: obj isReadOnlyObject
]
