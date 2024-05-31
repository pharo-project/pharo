Class {
	#name : 'TraitLegacyPharoClassDefinitionPrinterTest',
	#superclass : 'TraitCommonClassDefinitionPrinterTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'helpers' }
TraitLegacyPharoClassDefinitionPrinterTest >> forClass: aClass [
	^ (ClassDefinitionPrinter legacy for: aClass) definitionString
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self
		assert: (self forClass: AlignmentMorph class) equals: 'AlignmentMorph class
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testClassUsingComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object subclass: #MOPTestClassD
	uses: Trait2 @ {#c3->#c2}
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ' , self packageTagOfMOP
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: Trait4) equals: 'Trait named: #Trait4
	 uses: Trait1 + (Trait2 - {#c})
	 package: ' , self packageTagOfMOP
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testMultiPools [

	self
		assert: (self forClass: ClassMultiplePoolUser)
		equals: 'Object subclass: #ClassMultiplePoolUser
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''PoolDefiner PoolDefiner2''
	category: ''Kernel-Tests-Classes'''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTIsEmpty [
	"to me this looks strange to have empty uses here when empty instancevariablenames are not displayed."

	self assert: (self forClass: TIsEmpty) equals: 'Trait named: #TIsEmpty
	 uses: {}
	 package: ''Kernel-Traits-Base'''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTSortable [

	self assert: (self forClass: TSortable classTrait) equals: 'TSortable classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTrait [

	self assert: (self forClass: RubScrolledTextModel) equals: 'Object subclass: #RubScrolledTextModel
	uses: TViewModel
	instanceVariableNames: ''hasUnacceptedEdits announcer text primarySelectionInterval interactionModel''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Rubric-Editing-Widgets'''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitLegacyPharoClassDefinitionPrinterTest >> testTrait3AndTag [

	self assert: (self forClass: Trait3) equals: 'Trait named: #Trait3
	 uses: Trait2
	 package: ' , self packageTagOfMOP
]
