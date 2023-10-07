Class {
	#name : 'CDClassParserSelectionTest',
	#superclass : 'TestCase',
	#instVars : [
		'slotuse'
	],
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'running' }
CDClassParserSelectionTest >> setUp [
	super setUp.
	slotuse := ClassDefinitionPrinter showFluidClassDefinition
]

{ #category : 'running' }
CDClassParserSelectionTest >> tearDown [

	ClassDefinitionPrinter showFluidClassDefinition: slotuse.
	super tearDown
]

{ #category : 'tests' }
CDClassParserSelectionTest >> testParsingAClassWhenFluidSyntaxIsOnShouldSelectTheFluidParser [

	"we do not get a complex class"
 	self deny: self class needsSlotClassDefinition.
	ClassDefinitionPrinter showFluidClassDefinition: true.
	self assert: ClassDefinitionPrinter showFluidClassDefinition.

	self
		assert: (self class definitionString)
		equals:
	  'TestCase << #CDClassParserSelectionTest
	slots: { #slotuse };
	package: ''ClassParser-Tests'''.
	"ok so far so good"

	self assert: self class classDefinitionParserClass equals: CDFluidClassDefinitionParser.
	"here we cannot invoke the definition printer because it is chosen without asking the class so far."
]

{ #category : 'tests' }
CDClassParserSelectionTest >> testParsingAFluidClassShouldSelectTheFluidParser [

	"we get a complex class"
	self assert: MockWithComplexSlot needsSlotClassDefinition.
	self
		assert: (MockWithComplexSlot definitionString)
		equals:
	  'Object << #MockWithComplexSlot
	slots: { #aComplexSlot => ObservableSlot };
	tag: ''Fluid'';
	package: ''Kernel-Tests'''.
	"ok so far so good"

	self assert: MockWithComplexSlot classDefinitionParserClass equals: CDFluidClassDefinitionParser.
	"here we cannot invoke the definition printer because it is chosen without asking the class so far."
]

{ #category : 'tests' }
CDClassParserSelectionTest >> testParsingAFluidClassShouldSelectTheFluidParserEvenIfFluidIsOff [

	"we get a complex class"
	self assert: MockWithComplexSlot needsSlotClassDefinition.
	ClassDefinitionPrinter showFluidClassDefinition: false.
	self deny: 	ClassDefinitionPrinter showFluidClassDefinition.
	self
		assert: (MockWithComplexSlot definitionString)
		equals:
	  'Object << #MockWithComplexSlot
	slots: { #aComplexSlot => ObservableSlot };
	tag: ''Fluid'';
	package: ''Kernel-Tests'''.
	"ok so far so good"

	self assert: MockWithComplexSlot classDefinitionParserClass equals: CDFluidClassDefinitionParser.
	"here we cannot invoke the definition printer because it is chosen without asking the class so far."
]

{ #category : 'tests' }
CDClassParserSelectionTest >> testParsingANonComplexClassWhenFluidSyntaxOff [

	"we do not get a complex class"
 	self deny: self class needsSlotClassDefinition.
	ClassDefinitionPrinter showFluidClassDefinition: false.
	self deny: ClassDefinitionPrinter showFluidClassDefinition.

	self
		assert: (self class definitionString)
		equals:
	  'TestCase subclass: #CDClassParserSelectionTest
	instanceVariableNames: ''slotuse''
	classVariableNames: ''''
	package: ''ClassParser-Tests'''.
	"ok so far so good"

	self assert: self class classDefinitionParserClass equals: CDClassDefinitionParser.
	"here we cannot invoke the definition printer because it is chosen without asking the class so far."
]
