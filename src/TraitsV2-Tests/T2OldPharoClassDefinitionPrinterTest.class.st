Class {
	#name : #T2OldPharoClassDefinitionPrinterTest,
	#superclass : #T2CommonClassDefinitionPrinterTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #helpers }
T2OldPharoClassDefinitionPrinterTest >> forClass: aClass [
	^ (OldPharoClassDefinitionPrinter for: aClass) definitionString
]

{ #category : #running }
T2OldPharoClassDefinitionPrinterTest >> setUp [

	super setUp.
	ClassDefinitionPrinter showFluidClassDefinition: false
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self
		assert: (self forClass: AlignmentMorph class) equals: 'AlignmentMorph class
	uses: TAbleToRotate classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testClassDefinitionWithComplexSlotShouldBeDisplayedAsFluid [

	ClassDefinitionPrinter showFluidClassDefinition: false.
	self assert: TMockWithComplexSlot needsSlotClassDefinition.

	self
		assert: (self forClass: TMockWithComplexSlot)
		equals: 'Trait << #TMockWithComplexSlot
	slots: { #complexSlot => ObservableSlot };
	package: ''TraitsV2-Tests'''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object subclass: #MOPTestClassD
	uses: Trait2 @ {#c3->#c2}
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Tests-Traits-MOP'''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testMultiPools [

	self assert: (self forClass: ClassMultiplePoolUser) equals: 'Object subclass: #ClassMultiplePoolUser
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''PoolDefiner PoolDefiner2''
	package: ''Kernel-Tests-Classes'''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTBehavior [
	"to me this looks strange to have empty uses here when empty instancevariablenames are not displayed."
	self
		assert: (self forClass: TBehavior)  equals:  'Trait named: #TBehavior
	instanceVariableNames: ''''
	package: ''TraitsV2-Compatibility-Traits'''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTMockWithUseAndSlot [

	ClassDefinitionPrinter showFluidClassDefinition: false.
	self deny: TMockWithUseAndSlot needsSlotClassDefinition.
	"if the slot would be complex automatically the class def would switch to the fluid one
	So the test really test that the class creation API of the old syntax is correct.
	Even if we should push for using the fluid one."

	self
		assert: (self forClass: TMockWithUseAndSlot)
		equals: 'Trait named: #TMockWithUseAndSlot
	uses: Trait3
	instanceVariableNames: ''simpleSlot''
	package: ''TraitsV2-Tests'''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTSortable [

	self assert: (self forClass: TSortable classTrait) equals: 'TSortable classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTrait [

	self assert: (self forClass: RubScrolledTextModel) equals: 'Object subclass: #RubScrolledTextModel
	uses: TViewModel
	instanceVariableNames: ''hasUnacceptedEdits announcer text primarySelectionInterval interactionModel''
	classVariableNames: ''''
	package: ''Rubric-Editing-Widgets'''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	uses: Trait2 classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTrait3AndTag [

	self
		assert: (self forClass: Trait3)
		equals:
 'Trait named: #Trait3
	uses: Trait2
	instanceVariableNames: ''''
	package: ''Tests-Traits-MOP'''
]
