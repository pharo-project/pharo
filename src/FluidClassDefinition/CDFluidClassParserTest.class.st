Class {
	#name : #CDFluidClassParserTest,
	#superclass : #TestCase,
	#instVars : [
		'classDefinition'
	],
	#category : #'FluidClassDefinition-Parser'
}

{ #category : #running }
CDFluidClassParserTest >> classDefinitionParserClass [

	^ CDFluidClassDefinitionParser
]

{ #category : #'tests - (r) simple class definition' }
CDFluidClassParserTest >> testClassSideDefinitionIsClassSide [

	| def |
	def := self classDefinitionParserClass parse: 'Point class
			slot: {  }'.

	self assert: def isClassSideDefinition 
]

{ #category : #'tests - (r) class side' }
CDFluidClassParserTest >> testClassSideWitTraits [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'AlignmentMorph class
		uses: TableRotate classTrait;
		slots: { #x . #y}'.
		
	def := parser parse: defString.
	self assert: def class equals: CDMetaclassDefinitionNode.
	self assert: def hasTraitComposition.
	self assert: def traitDefinition class equals: CDClassTraitNode.
	self assert: def traitDefinition name equals: #TableRotate.
	self assert: def slots first name equals: #x
]

{ #category : #'tests - (r) class variables' }
CDFluidClassParserTest >> testClassVariables [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		sharedVariables: { #A . #B };
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def sharedSlots first name equals: #A. 
	self assert: def sharedSlots second name equals: #B. 
]

{ #category : #'tests - (r) slots' }
CDFluidClassParserTest >> testComplexSlots [

	| parser defString def slot |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		slots: { #inst => (InstanceVariableSlot default: 5) };
		package: #MyPackage'.
	def := parser parse: defString.
	slot := def slots first.
	self assert: slot name equals: #inst. 
	self assert: slot initializationMessage selectorParts equals: #(#default:).
	self assert: (slot initializationMessage argumentParts collect: [:each | each value]) equals: #(5) asOrderedCollection
]

{ #category : #'tests - (r) slots' }
CDFluidClassParserTest >> testEmptySlots [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		slots: {};
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def slots isEmpty

]

{ #category : #'tests - (r) kinds' }
CDFluidClassParserTest >> testEphemeronSubclass [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		layout: EphemeronLayout;
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def classKind equals: #ephemeron
]

{ #category : #'tests - (r) simple class definition' }
CDFluidClassParserTest >> testInstanceDefinitionIsInstanceSide [

	| def |
	def := self classDefinitionParserClass parse: 'Object < #Point
			package: ''Kernel-BasicObjects'''.

	self assert: def isInstanceSideDefinition 
]

{ #category : #'tests - (r) kinds' }
CDFluidClassParserTest >> testNormalSubclass [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		layout: FixedLayout;
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def classKind equals: #normal
]

{ #category : #'tests - (r) sharedPools' }
CDFluidClassParserTest >> testSharedPools [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
	   sharedPools: {TextConstants};
		package: #MyPackage'.
	
	def := parser parse: defString.
	self assert: def sharedPools first name equals: 'TextConstants'
]

{ #category : #'tests - (r) simple class definition' }
CDFluidClassParserTest >> testSimpleDefinition [

	| def |
	def := self classDefinitionParserClass parse: 'Object < #Point
			package: ''Kernel-BasicObjects'''.
			
	self assert: def className equals: #Point.

]

{ #category : #'tests - (r) simple class definition' }
CDFluidClassParserTest >> testSimpleDefinitionClassNode [

	| def |
	def := self classDefinitionParserClass parse: 'Object < #Point
			package: ''Kernel-BasicObjects'''.

	self assert: def classNameNode className equals: #Point.
	
	"The following cannot work 
	
		self assert: def classNameNode binding value equals: Point. 
	
	because binding is defined as 
	
		existingBindingIfAbsent: aBlock

			| binding |
			binding := originalNode methodNode compilationContext environment bindingOf: className.
			^ binding ifNil: aBlock
	
	"

]

{ #category : #'tests - (r) simple class definition' }
CDFluidClassParserTest >> testSimpleDefinitionPackageIsCorrect [
	
	| def |
	def := self classDefinitionParserClass parse: 'Object < #Point
			package: ''Kernel-BasicObjects'''.
	self assert: def packageName equals: 'Kernel-BasicObjects'
]

{ #category : #'tests - (r) simple class definition' }
CDFluidClassParserTest >> testSimpleDefinitionSuperclassName [

	| def |
	def := self classDefinitionParserClass parse: 'Object < #Point
			package: ''Kernel-BasicObjects'''.
			
	self assert: def superclassName equals: 'Object'
	
]

{ #category : #'tests - (r) slots' }
CDFluidClassParserTest >> testSimpleSlots [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		slots: { #a. #b };
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def slots size equals: 2.
	self assert: def slots first name equals: #a. 
	self assert: def slots second name equals: #b. 
]

{ #category : #'tests - (r) tags' }
CDFluidClassParserTest >> testTag [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	defString := 'Object < #MyObject
		tag: ''tag1'';
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def tag name equals: 'tag1'
]

{ #category : #'tests - (r) traits' }
CDFluidClassParserTest >> testTraitAlias [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		uses: MyTrait @ {#foo -> #bar};
		package: #MyPackage'.
	
	def := parser parse: defString.
	self assert: def traitDefinition class equals: CDTraitAliasNode. 
	self assert: (def traitDefinition aliases values) equals: #(bar).
	self assert: (def traitDefinition aliases keys) equals: #(foo).
	self assert: def traitDefinition subject name equals: #MyTrait. 	

]

{ #category : #'tests - (r) traits' }
CDFluidClassParserTest >> testTraitEmpty [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		uses: {};
		package: #MyPackage'.
		
	def := parser parse: defString.
	self assert: def traitDefinition equals: nil
]

{ #category : #'tests - (r) traits' }
CDFluidClassParserTest >> testTraitPlainSimple [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		uses: MyTrait;
		package: #MyPackage'.
		
	def := parser parse: defString.
	self assert: def traitDefinition name equals: #MyTrait.
]

{ #category : #'tests - (r) traits' }
CDFluidClassParserTest >> testTraitSequence [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	defString := 'Object < #MyObject
		uses: MyTrait + (AnotherTrait - {#selector} @ {#selector1 -> #selector});
		package: #MyPackage'.

	def := parser parse: defString.
	self assert: def traitDefinition class equals: CDTraitCompositionSequenceNode. 
	self assert: def traitDefinition sequence size equals: 2.
	self assert: (def traitDefinition sequence second aliases values) equals: #(selector).
	self assert: (def traitDefinition sequence second aliases keys) equals: #(selector1).
	self assert: def traitDefinition sequence first name equals: #MyTrait. 	

]

{ #category : #'tests - (r) slots' }
CDFluidClassParserTest >> testTypedSlots [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		slots: { #inst => InstanceVariableSlot };
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def slots size equals: 1.
	self assert: def slots first name equals: #inst. 
	
]

{ #category : #'tests - (r) kinds' }
CDFluidClassParserTest >> testVariableByteSubclass [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		layout: VariableLayout;
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def classKind equals: #variable
]

{ #category : #'tests - (r) kinds' }
CDFluidClassParserTest >> testVariableSubclass [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		layout: VariableLayout;
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def classKind equals: #variable
]

{ #category : #'tests - (r) kinds' }
CDFluidClassParserTest >> testVariableWordSubclass [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		layout: WordLayout;
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def classKind equals: #variableWord
]

{ #category : #'tests - (r) kinds' }
CDFluidClassParserTest >> testWeakSubclass [

	| parser defString def |
	parser := self classDefinitionParserClass new.
	
	defString := 'Object < #MyObject
		layout: WeakLayout;
		package: #MyPackage'.
	def := parser parse: defString.
	self assert: def classKind equals: #weak
]
