Class {
	#name : 'T2OldPharoClassDefinitionPrinterTest',
	#superclass : 'T2CommonClassDefinitionPrinterTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'helpers' }
T2OldPharoClassDefinitionPrinterTest >> forClass: aClass [
	^ (OldPharoClassDefinitionPrinter oldPharo for: aClass) definitionString
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self
		assert: (self forClass: AlignmentMorph class) equals: 'AlignmentMorph class
	uses: TAbleToRotate classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object subclass: #MOPTestClassD
	uses: Trait2 @ {#c3->#c2}
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ' , self packageTagOfMOP
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testMultiPools [

	self assert: (self forClass: ClassMultiplePoolUser) equals: 'Object subclass: #ClassMultiplePoolUser
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''PoolDefiner PoolDefiner2''
	package: ''Kernel-Tests-Classes'''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTBehavior [
	"to me this looks strange to have empty uses here when empty instancevariablenames are not displayed."
	self
		assert: (self forClass: TBehavior)  equals:  'Trait named: #TBehavior
	instanceVariableNames: ''''
	package: ''TraitsV2-Compatibility-Traits'''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTSortable [

	self assert: (self forClass: TSortable classTrait) equals: 'TSortable classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTrait [

	self assert: (self forClass: RubScrolledTextModel) equals: 'Object subclass: #RubScrolledTextModel
	uses: TViewModel
	instanceVariableNames: ''hasUnacceptedEdits announcer text primarySelectionInterval interactionModel''
	classVariableNames: ''''
	package: ''Rubric-Editing-Widgets'''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	uses: Trait2 classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests - traits' }
T2OldPharoClassDefinitionPrinterTest >> testTrait3AndTag [

	self assert: (self forClass: Trait3) equals: 'Trait named: #Trait3
	uses: Trait2
	instanceVariableNames: ''''
	package: ' , self packageTagOfMOP
]
