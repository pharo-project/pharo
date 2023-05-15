Class {
	#name : #T2LegacyPharoClassDefinitionPrinterTest,
	#superclass : #T2CommonClassDefinitionPrinterTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #helpers }
T2LegacyPharoClassDefinitionPrinterTest >> forClass: aClass [
	^ (ClassDefinitionPrinter legacy for: aClass) definitionString
]

{ #category : #running }
T2LegacyPharoClassDefinitionPrinterTest >> setUp [

	super setUp.
	ClassDefinitionPrinter showFluidClassDefinition: false
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self
		assert: (self forClass: AlignmentMorph class) equals: 'AlignmentMorph class
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testClassUsingComplexTrait [

	self
		assert: (self forClass: MOPTestClassD)
		equals: 'Object subclass: #MOPTestClassD
	uses: Trait2 @ {#c3->#c2}
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Tests-Traits-MOP'''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testComplexTrait [

	self
		assert: (self forClass: Trait4)
		equals: 'Trait named: #Trait4
	 uses: Trait1 + (Trait2 - {#c})
	 package: ''Tests-Traits-MOP'''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testMultiPools [

	self
		assert: (self forClass: ClassMultiplePoolUser)
		equals: 'Object subclass: #ClassMultiplePoolUser
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''PoolDefiner PoolDefiner2''
	category: ''Kernel-Tests-Classes'''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTBehavior [
	"to me this looks strange to have empty uses here when empty instancevariablenames are not displayed."
	self
		assert: (self forClass: TBehavior)  equals: 'Trait named: #TBehavior
	 uses: {}
	 package: ''TraitsV2-Compatibility-Traits'''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTSortable [

	self assert: (self forClass: TSortable classTrait) equals: 'TSortable classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTrait [

	self assert: (self forClass: RubScrolledTextModel) equals: 'Object subclass: #RubScrolledTextModel
	uses: TViewModel
	instanceVariableNames: ''hasUnacceptedEdits announcer text primarySelectionInterval interactionModel''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Rubric-Editing-Widgets'''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
T2LegacyPharoClassDefinitionPrinterTest >> testTrait3AndTag [

	self
		assert: (self forClass: Trait3)
		equals: 'Trait named: #Trait3
	 uses: Trait2
	 package: ''Tests-Traits-MOP'''
]
