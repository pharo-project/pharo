Class {
	#name : #OldPharoClassDefinitionPrinterTest,
	#superclass : #CommonClassDefinitionPrinterTest,
	#category : #'Kernel-Tests-Fluid'
}

{ #category : #helpers }
OldPharoClassDefinitionPrinterTest >> forClass: aClass [
	^ (OldPharoClassDefinitionPrinter for: aClass) definitionString 


]

{ #category : #running }
OldPharoClassDefinitionPrinterTest >> setUp [

	super setUp.
	ClassDefinitionPrinter showFluidClassDefinition: false
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testAlignmentMorphClass [

	self 
		assert: (self forClass: AlignmentMorph class) equals: 'AlignmentMorph class
	uses: TAbleToRotate classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - classes' }
OldPharoClassDefinitionPrinterTest >> testArray [

	self 
		assert: (self forClass: Array) 
		equals: 'ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Collections-Sequenceable-Base'''
]

{ #category : #'tests - classes' }
OldPharoClassDefinitionPrinterTest >> testByteString [

	self 
		assert: (self forClass: ByteString) 
		equals: 'String variableByteSubclass: #ByteString
	instanceVariableNames: ''''
	classVariableNames: ''NonAsciiMap''
	package: ''Collections-Strings-Base'''
]

{ #category : #'tests - classes' }
OldPharoClassDefinitionPrinterTest >> testChronologyConstants [
	
	self 
		assert: (self forClass: ChronologyConstants) 
		equals: 'SharedPool subclass: #ChronologyConstants
	instanceVariableNames: ''''
	classVariableNames: ''DayNames DaysInMonth MicrosecondsInDay MonthNames NanosInMillisecond NanosInSecond SecondsInDay SecondsInHour SecondsInMinute SqueakEpoch''
	package: ''Kernel-Chronology'''
]

{ #category : #'tests - metaclasses' }
OldPharoClassDefinitionPrinterTest >> testClassSideDoesNotShowPackage [

	self assert: (self forClass: MOPTraitTest class) equals: 'MOPTraitTest class
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testComplexTrait [

	self assert: (self forClass: MOPTestClassD) equals: 'Object subclass: #MOPTestClassD
	uses: Trait2 @ {#c3->#c2}
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Tests-Traits-MOP'''

]

{ #category : #'tests - metaclasses' }
OldPharoClassDefinitionPrinterTest >> testMetaclass [
	
	self assert: (self forClass: Metaclass) equals:  'ClassDescription subclass: #Metaclass
	instanceVariableNames: ''thisClass''
	classVariableNames: ''''
	package: ''Kernel-Classes'''
]

{ #category : #'tests - metaclasses' }
OldPharoClassDefinitionPrinterTest >> testMetaclassClass [
	
	self 
		assert: (self forClass: Metaclass class) 
		equals: 'Metaclass class
	instanceVariableNames: '''''	
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testMultiPools [

	self assert: (self forClass: ClassMultiplePoolUser) equals: 'Object subclass: #ClassMultiplePoolUser
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''PoolDefiner PoolDefiner2''
	package: ''Kernel-Tests-Classes'''
]

{ #category : #'tests - classes' }
OldPharoClassDefinitionPrinterTest >> testPoint [

	self 
		assert: (self forClass: Point) 
		equals: 'Object subclass: #Point
	instanceVariableNames: ''x y''
	classVariableNames: ''''
	package: ''Kernel-BasicObjects'''
]

{ #category : #'tests - classes' }
OldPharoClassDefinitionPrinterTest >> testProtoObject [
	
	self assert: (self forClass: ProtoObject) equals: 'ProtoObject subclass: #ProtoObject
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Kernel-Objects''.
ProtoObject superclass: nil'
		
 
]

{ #category : #'tests - metaclasses' }
OldPharoClassDefinitionPrinterTest >> testSystemAnnouncerClass [
	
	self 
		assert: (self forClass: SystemAnnouncer class) 
		equals: 'SystemAnnouncer class
	instanceVariableNames: ''announcer'''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTBehavior [
	"to me this looks strange to have empty uses here when empty instancevariablenames are not displayed."
	self 
		assert: (self forClass: TBehavior)  equals: 'Trait named: #TBehavior
	 uses: {}
	 package: ''TraitsV2-Compatibility-Traits'''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTComparableClassTrait [

	self assert: (self forClass: TComparable classTrait) equals: 'TComparable classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTEventVisitorClassTrait [

	self assert: (self forClass: EpTEventVisitor classTrait) equals: 'EpTEventVisitor classTrait
	uses: EpTCodeChangeVisitor classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTSortable [

	self assert: (self forClass: TSortable classTrait) equals: 'TSortable classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - classes' }
OldPharoClassDefinitionPrinterTest >> testText [

	self assert: (self forClass: Text) equals: 'ArrayedCollection subclass: #Text
	instanceVariableNames: ''string runs''
	classVariableNames: ''''
	poolDictionaries: ''TextConstants''
	package: ''Text-Core-Base'''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTrait [
	
	self assert: (self forClass: RubScrolledTextModel) equals: 'Object subclass: #RubScrolledTextModel
	uses: TViewModel
	instanceVariableNames: ''hasUnacceptedEdits announcer text primarySelectionInterval interactionModel''
	classVariableNames: ''''
	package: ''Rubric-Editing-Widgets'''
	 
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTrait3 [

	self assert: (self forClass: Trait3 classTrait) equals: 'Trait3 classTrait
	uses: Trait2 classTrait
	instanceVariableNames: '''''
]

{ #category : #'tests - traits' }
OldPharoClassDefinitionPrinterTest >> testTrait3AndTag [

	self 
		assert: (self forClass: Trait3) 
		equals: 'Trait named: #Trait3
	 uses: Trait2
	 package: ''Tests-Traits-MOP''' 
]
