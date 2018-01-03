Class {
	#name : #FFIExternalStructureFieldParserTests,
	#superclass : #TestCase,
	#instVars : [
		'architecture'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #factory }
FFIExternalStructureFieldParserTests >> newParser [   
	^ FFIExternalStructureFieldParser new
]

{ #category : #running }
FFIExternalStructureFieldParserTests >> setUp [
	super setUp.
	architecture := FFIArchitecture forCurrentArchitecture
]

{ #category : #tests }
FFIExternalStructureFieldParserTests >> testParseFieldsStructure [
	| fieldSpec |
	
	fieldSpec := self newParser   
		parseFields: FFITestStructure fields 
		structure: FFITestStructure.
		
	self assert: fieldSpec notNil. 
	self assert: fieldSpec fieldNames equals: #(byte short long float double int64 ulong).
	self assert: (fieldSpec typeFor: #byte) class equals: FFIUInt8.
	self assert: (fieldSpec typeFor: #short) class equals: FFIInt16.
	self assert: (fieldSpec typeFor: #long) class equals: FFILong.
	self assert: (fieldSpec typeFor: #float) class equals: FFIFloat32.
	self assert: (fieldSpec typeFor: #double) class equals: FFIFloat64.
	self assert: (fieldSpec typeFor: #int64) class equals: FFIInt64.
	self assert: (fieldSpec typeFor: #ulong) class equals: FFIULong.
	
]
