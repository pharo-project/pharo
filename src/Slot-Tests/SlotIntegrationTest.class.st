"
I'm a test case of SlotClassBuilder integration in the system. Tipically, my tests assert over Class API.
"
Class {
	#name : #SlotIntegrationTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-ClassBuilder'
}

{ #category : #tests }
SlotIntegrationTest >> testAddAndAddInstVarNamedWithTrait2 [
	"Add instance variables using the builder interface"
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			traitComposition: TOne;
			slots: #( x )
		].

	self assertTraitIntegrity.
	self assert: aClass instVarNames equals: #(x).
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1.

	aClass addInstVarNamed: #y.
	self assertTraitIntegrity.
	aClass addInstVarNamed: #z.
	
	self assert: aClass instVarNames equals: #(x y z).
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1
]

{ #category : #tests }
SlotIntegrationTest >> testAddAndRemoveInstVarNamedWithTrait2 [
	"Add instance variables using the builder interface"
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			traitComposition: TOne;
			slots: #( x )
		].
	
	self assertTraitIntegrity.
	self assert: aClass instVarNames equals: #(x).
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1.

	aClass addInstVarNamed: #y.
	self assertTraitIntegrity.
	aClass	removeInstVarNamed: #x.
	
	self assert: aClass instVarNames equals: #(y).
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1
]

{ #category : #tests }
SlotIntegrationTest >> testAddInstVarNamed [
	"Add instance variables using the builder interface"
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #( x )
		].

	self assert: aClass instVarNames equals: #(x).	


	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName;
			slots: #( y )
		].
	
	self assert: anotherClass instVarNames equals: #(y).
]

{ #category : #tests }
SlotIntegrationTest >> testAddInstVarNamedClassInterface [

	"Add instance variables using the class interface"
	
	aClass := self make: [ :builder |
		builder name: self aClassName 
		].
	aClass addInstVarNamed: 'x'.

	self assert: aClass instVarNames equals: #(x).	


	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName.
		].
	anotherClass addInstVarNamed: 'y'.
	
	self assert: anotherClass instVarNames equals: #(y)
]

{ #category : #tests }
SlotIntegrationTest >> testAddInstVarNamedWithTrait [
	"Add instance variables using the builder interface"

	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				traitComposition: TOne;
				slots: #(x) ].
	self assertTraitIntegrity.
	self assert: aClass instVarNames equals: #(x).

	self assert: TOne traitUsers asArray equals: {aClass}.

	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1.

	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				traitComposition: TOne;
				slots: #(x y) ].

	self assertTraitIntegrity.
	self assert: aClass instVarNames equals: #(x y).

	self assert: TOne traitUsers asArray equals: {aClass}.

	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1
]

{ #category : #tests }
SlotIntegrationTest >> testAddInstVarNamedWithTrait2 [
	"Add instance variables using the builder interface"
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			traitComposition: TOne;
			slots: #( x )
		].

	self assertTraitIntegrity.
	self assert: aClass instVarNames equals: #(x).
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1.

	aClass addInstVarNamed: #y.
	
	self assertTraitIntegrity.
	self assert: aClass instVarNames equals: #(x y).
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1
]

{ #category : #tests }
SlotIntegrationTest >> testAnonymousSubclass [
	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				slots: #(x) ].

	anotherClass := aClass newAnonymousSubclass.
	self deny: anotherClass identicalTo: aClass.
	self assert: anotherClass isBehavior.
	self assert: anotherClass superclass identicalTo: aClass.
	self deny: anotherClass name equals: aClass name.
	self assert: anotherClass allInstVarNames equals: #(x)
]

{ #category : #tests }
SlotIntegrationTest >> testCompiledMethodLayout [
	self assert: (CompiledMethod classLayout isKindOf: CompiledMethodLayout)
]

{ #category : #tests }
SlotIntegrationTest >> testRemoveInstVarNamed [

	| answer |

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(a) 
		].

	answer := 
		aClass 
			slotNamed: 'a' 
			ifFound: [ #found ] 
			ifNone: [ #none ].
	self assert: answer equals: #found.

	answer := 
		aClass 
			slotNamed: 'b' 
			ifFound: [ #found ] 
			ifNone: [ #none ].
	self assert: answer equals: #none

]

{ #category : #tests }
SlotIntegrationTest >> testRemoveInstVarNamedClassInterface [

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(x y z) 
		].
	aClass removeInstVarNamed: #y.

	self assert: aClass instVarNames equals: #(x z).
]

{ #category : #tests }
SlotIntegrationTest >> testRemoveInstVarNamedWithTrait2 [
	"Add instance variables using the builder interface"

	aClass := self
		make: [ :builder | 
			builder
				name: self aClassName;
				traitComposition: TOne;
				slots: #(x) ].

	self assert: aClass instVarNames equals: #(x).

	self assert: TOne traitUsers asArray equals: {aClass}.

	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1.

	aClass removeInstVarNamed: #x.

	self assertEmpty: aClass instVarNames.

	self assert: TOne traitUsers asArray equals: {aClass}.

	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1
]

{ #category : #tests }
SlotIntegrationTest >> testReshapeClassPropagatesToDeepHierarchyClassInterface [
	"Test reshaping a class which is head of a hierarchy of 4 levels"

	"level 1"

	aClass := Object
		subclass: self aClassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assertEmpty: aClass subclasses.
	aClass classLayout checkIntegrity.

	"level 2"
	anotherClass := aClass
		subclass: self anotherClassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: aClass subclasses equals: {anotherClass}.
	self assertEmpty: anotherClass subclasses.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.

	"level 3"
	yetAnotherClass := anotherClass
		subclass: self yetAnotherClassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: aClass subclasses equals: {anotherClass}.
	self assert: anotherClass subclasses equals: {yetAnotherClass}.
	self assertEmpty: yetAnotherClass subclasses.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.

	"level 4"
	yetYetAnotherClass := yetAnotherClass
		subclass: self yetYetAnotherClassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: aClass subclasses equals: {anotherClass}.
	self assert: anotherClass subclasses equals: {yetAnotherClass}.
	self assert: yetAnotherClass subclasses equals: {yetYetAnotherClass}.
	self assertEmpty: yetYetAnotherClass subclasses.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity.

	"reshape level 1"
	aClass := Object
		subclass: self aClassName
		instanceVariableNames: 'x'
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: aClass instVarNames equals: #(x).
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity
]

{ #category : #tests }
SlotIntegrationTest >> testReshapeClassWithClassSlot [
	
	aClass := 
		Object 
			subclass: self aClassName 
			instanceVariableNames: '' 
			classVariableNames: '' 
			poolDictionaries: ''
			category: self aCategory.	
	aClass class 
		instanceVariableNames: #x.
	
	self assert: aClass class instVarNames equals: #(x).
	
	aClass := 
		Object 
			subclass: self aClassName 
			instanceVariableNames: 'a' 
			classVariableNames: '' 
			poolDictionaries: ''
			category: self aCategory.	

	self assert: aClass class instVarNames equals: #(x).
]

{ #category : #tests }
SlotIntegrationTest >> testSlotNamedIfFoundIfNone [

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(a) 
		].

	self 
		assert: (aClass slotNamed: 'a' ifFound: [ #found ] ifNone: [ #none ])
		equals: #found.

	self 
		assert: (aClass slotNamed: 'b' ifFound: [ #found ] ifNone: [ #none ])
		equals: #none

]

{ #category : #tests }
SlotIntegrationTest >> testSlotScopeParallelism [
	"Proposed by Camille Teruel"

	| classesAndMetaclasses candidates classWithWrongSlotScope |
	classesAndMetaclasses := Smalltalk allClasses flatCollect: [ :each | { each. each class } ].

	candidates := classesAndMetaclasses reject: [ :each | 
		each superclass isNil or: [ 
			each classLayout slotScope isKindOf: LayoutEmptyScope ] ].

	classWithWrongSlotScope := candidates reject: [ :each | 
		each superclass classLayout slotScope == each classLayout slotScope parentScope ].

	self assert: classWithWrongSlotScope asArray equals: #()
]

{ #category : #tests }
SlotIntegrationTest >> testSlotsAreInitializedWithDefiningAnonimousClass [
	"All slots should include reference to defining class"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(x)
		].
	self assert: aClass slots first owningClass equals: aClass.
	
	aClass addInstVarNamed: 'y'.
	self assert: aClass slots size equals: 2.
	self assert: (aClass slots collect: #owningClass as: Set) equals: {aClass} asSet
]

{ #category : #tests }
SlotIntegrationTest >> testSmallIntegerLayout [
	self assert: (SmallInteger classLayout isKindOf: ImmediateLayout)
]
