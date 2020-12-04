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
OldPharoClassDefinitionPrinterTest >> testArrayedCollectionWithPoolDictionary [

	self assert: (self forClass: ExampleForTestWithSharedPool) equals: 'ArrayedCollection subclass: #ExampleForTestWithSharedPool
	instanceVariableNames: ''aSlot anotherSlot''
	classVariableNames: ''''
	poolDictionaries: ''ChronologyConstants''
	package: ''Kernel-Tests-Classes'''
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

	self assert: (self forClass: ContextTest class) equals: 'ContextTest class
	instanceVariableNames: '''''
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
