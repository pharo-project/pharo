Class {
	#name : #AbstractOldPrinterTest,
	#superclass : #CommonClassDefinitionPrinterTest,
	#category : #'Kernel-Tests-Fluid'
}

{ #category : #helpers }
AbstractOldPrinterTest >> forClass: aClass [
	^ (OldPharoClassDefinitionPrinter for: aClass) definitionString 


]

{ #category : #running }
AbstractOldPrinterTest >> setUp [

	super setUp.
	ClassDefinitionPrinter showFluidClassDefinition: false
]

{ #category : #'tests - classes' }
AbstractOldPrinterTest >> testArray [

	self 
		assert: (self forClass: Array) 
		equals: 'ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Collections-Sequenceable-Base'''
]

{ #category : #'tests - classes' }
AbstractOldPrinterTest >> testArrayedCollectionWithPoolDictionary [

	self assert: (self forClass: ExampleForTestWithSharedPool) equals: 'ArrayedCollection subclass: #ExampleForTestWithSharedPool
	instanceVariableNames: ''aSlot anotherSlot''
	classVariableNames: ''''
	poolDictionaries: ''ChronologyConstants''
	package: ''Kernel-Tests-Classes'''
]

{ #category : #'tests - classes' }
AbstractOldPrinterTest >> testByteString [

	self 
		assert: (self forClass: ByteString) 
		equals: 'String variableByteSubclass: #ByteString
	instanceVariableNames: ''''
	classVariableNames: ''NonAsciiMap''
	package: ''Collections-Strings-Base'''
]

{ #category : #'tests - classes' }
AbstractOldPrinterTest >> testChronologyConstants [
	
	self 
		assert: (self forClass: ChronologyConstants) 
		equals: 'SharedPool subclass: #ChronologyConstants
	instanceVariableNames: ''''
	classVariableNames: ''DayNames DaysInMonth MicrosecondsInDay MonthNames NanosInMillisecond NanosInSecond SecondsInDay SecondsInHour SecondsInMinute SqueakEpoch''
	package: ''Kernel-Chronology'''
]

{ #category : #'tests - metaclasses' }
AbstractOldPrinterTest >> testClassSideDoesNotShowPackage [

	self assert: (self forClass: ContextTest class) equals: 'ContextTest class
	instanceVariableNames: '''''
]

{ #category : #'tests - metaclasses' }
AbstractOldPrinterTest >> testMetaclass [
	
	self assert: (self forClass: Metaclass) equals:  'ClassDescription subclass: #Metaclass
	instanceVariableNames: ''thisClass''
	classVariableNames: ''''
	package: ''Kernel-Classes'''
]

{ #category : #'tests - metaclasses' }
AbstractOldPrinterTest >> testMetaclassClass [
	
	self 
		assert: (self forClass: Metaclass class) 
		equals: 'Metaclass class
	instanceVariableNames: '''''	
]

{ #category : #'tests - classes' }
AbstractOldPrinterTest >> testPoint [

	self 
		assert: (self forClass: Point) 
		equals: 'Object subclass: #Point
	instanceVariableNames: ''x y''
	classVariableNames: ''''
	package: ''Kernel-BasicObjects'''
]

{ #category : #'tests - classes' }
AbstractOldPrinterTest >> testProtoObject [
	
	self assert: (self forClass: ProtoObject) equals: 'ProtoObject subclass: #ProtoObject
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Kernel-Objects''.
ProtoObject superclass: nil'
		
 
]

{ #category : #'tests - metaclasses' }
AbstractOldPrinterTest >> testSystemAnnouncerClass [
	
	self 
		assert: (self forClass: SystemAnnouncer class) 
		equals: 'SystemAnnouncer class
	instanceVariableNames: ''announcer'''
]
