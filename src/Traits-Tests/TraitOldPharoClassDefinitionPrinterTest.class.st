Class {
	#name : 'TraitOldPharoClassDefinitionPrinterTest',
	#superclass : 'TraitCommonClassDefinitionPrinterTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'helpers' }
TraitOldPharoClassDefinitionPrinterTest >> forClass: aClass [
	^ (OldPharoClassDefinitionPrinter oldPharo for: aClass) definitionString
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self
		assert: (self forClass: AlignmentMorph class) equals: 'AlignmentMorph class
	uses: TAbleToRotate classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object subclass: #MOPTestClassD
	uses: Trait2 @ {#c3->#c2}
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ' , self packageTagOfMOP
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testMultiPools [

	self assert: (self forClass: ClassMultiplePoolUser) equals: 'Object subclass: #ClassMultiplePoolUser
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''PoolDefiner PoolDefiner2''
	package: ''Kernel-Tests-Classes'''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTIsEmpty [
	"to me this looks strange to have empty uses here when empty instancevariablenames are not displayed."

	self assert: (self forClass: TIsEmpty) equals: 'Trait named: #TIsEmpty
	instanceVariableNames: ''''
	package: ''Kernel-Traits-Base'''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTSortable [

	self assert: (self forClass: TSortable classTrait) equals: 'TSortable classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTrait [

	self assert: (self forClass: RubScrolledTextModel) equals: 'Object subclass: #RubScrolledTextModel
	uses: TViewModel
	instanceVariableNames: ''hasUnacceptedEdits announcer text primarySelectionInterval interactionModel''
	classVariableNames: ''''
	package: ''Rubric-Editing-Widgets'''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	uses: Trait2 classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
TraitOldPharoClassDefinitionPrinterTest >> testTrait3AndTag [

	self assert: (self forClass: Trait3) equals: 'Trait named: #Trait3
	uses: Trait2
	instanceVariableNames: ''''
	package: ' , self packageTagOfMOP
]
