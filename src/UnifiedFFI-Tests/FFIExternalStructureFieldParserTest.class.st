"
Tests for FFIExternalStructureFieldParser
"
Class {
	#name : #FFIExternalStructureFieldParserTest,
	#superclass : #TestCase,
	#instVars : [
		'architecture'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #factory }
FFIExternalStructureFieldParserTest >> newParser [   
	^ FFIExternalStructureFieldParser new
]

{ #category : #running }
FFIExternalStructureFieldParserTest >> setUp [
	super setUp.
	architecture := FFIArchitecture forCurrentArchitecture
]

{ #category : #tests }
FFIExternalStructureFieldParserTest >> testParseFieldsStructure [
	| fieldSpec |
	
	fieldSpec := self newParser   
		parseFields: FFITestStructure fields 
		structure: FFITestStructure.
		
	self assert: fieldSpec notNil. 
	self assert: fieldSpec fieldNames equals: #(byte short long float double int64 ulong size_t).
	self assert: (fieldSpec typeFor: #byte) class equals: FFIUInt8.
	self assert: (fieldSpec typeFor: #short) class equals: FFIInt16.
	self assert: (fieldSpec typeFor: #long) class equals: FFILong.
	self assert: (fieldSpec typeFor: #float) class equals: FFIFloat32.
	self assert: (fieldSpec typeFor: #double) class equals: FFIFloat64.
	self assert: (fieldSpec typeFor: #int64) class equals: FFIInt64.
	self assert: (fieldSpec typeFor: #ulong) class equals: FFIULong.
	self assert: (fieldSpec typeFor: #size_t) class equals: FFISizeT
	
]
